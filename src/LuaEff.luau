--!native
--!optimize 2
--!strict

--- LuaEff: Multi-shot, higher-order effects for Luau

--[[
    MIT License

    Copyright (c) 2026 Yargrimm

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
--]]

-- stylua: ignore start

-- A remider of the signedness of effect IDs
type i32 = number

export type RetNode  = { tag: i32, val: any }
export type EffNode  = { tag: i32, eid: i32, payload: any, k: Cont, s: SubComputations? }
export type Node     = RetNode | EffNode

-- Recursive types cannot have parameters.
--     type ContFunc<T> = (T) -> Node 
export type ContFunc        = (any) -> Node
export type ContPair        = { l: Cont, r: Cont }
export type Cont            = ContFunc | ContPair | nil
export type SubComputations = { Node }

export type Handler = {
    ["return"]: ((Handler, any) -> Node)?,
    methods: { [number]: (Handler, any, ContFunc) -> Node },
    [string]: (Handler, any, ContFunc) -> Node 
  }

export type Elaborator = {
  methods: { [number]: (Elaborator, any, SubComputations?, ContFunc) -> Node},
  [string]: (Elaborator, any, SubComputations?, ContFunc) -> Node 
}

type _BranchCtx = {
  results: { any },
  pending: i32,
  resolve: ({ any }) -> (),
  reject: (any) -> (),
}

type _ApplyCtx = {
  results: { any },
  pending: number,

}

local TAG_RET        = 0
local TAG_EFF        = 1
local table_create   = table.create
local table_freeze   = table.freeze

local EID_APPLY: i32 = -1
local EID_IO:    i32 =  1
local EID_FAIL:  i32 =  2
local EID_ASYNC: i32 =  3
local EID_NDET:  i32 =  4

local idToEffectName: { string } = {
  "io",
  "fail",
  "async",
  "ndet",
}
local effectNameToId: { [string]: number } = {}
for i, name in idToEffectName do
  effectNameToId[name] = i
end
local hoIdToEffectName: { string } = {
  "apply",
}
local hoEffectNameToId: { [string]: number } = {}
for i, name in hoIdToEffectName do
  hoEffectNameToId[name] = i
end

local _nid: i32 = 4
local _hoNid: i32 = -1

-- stylua: ignore end

local POOL_CAP: i32 = 32
local DEFAULT_SIZE = 16
local _pool: { { any } } = table_create(POOL_CAP) :: { { any } }
local _poolN: i32 = 0

for _ = 1, 8 do
	_poolN += 1
	_pool[_poolN] = table_create(DEFAULT_SIZE)
end

local function _acq(): { any }
	if _poolN > 0 then
		local s = _pool[_poolN]
		_pool[_poolN] = nil :: any
		_poolN -= 1
		return s
	end
	return table_create(DEFAULT_SIZE)
end

local function _rel(s: { any })
	if _poolN < POOL_CAP then
		table.clear(s)
		_poolN += 1
		_pool[_poolN] = s
	end
end

local function ret(v: any): RetNode
	return { tag = TAG_RET, val = v }
end

local function getEffectName(id: i32): string
	if id > 0 then
		return idToEffectName[id]
	end
	return hoIdToEffectName[-id]
end

local function mkEff(eid: i32, p: any, k: Cont, s: SubComputations?): EffNode
	if s then
		return { tag = TAG_EFF, eid = eid, payload = p, k = k, s = s }
	end
	return { tag = TAG_EFF, eid = eid, payload = p, k = k }
end

-- Curry op
local function op(name: string, payload: any?): EffNode
	-- NOTE: When an operation is created with a nil continuation,
	-- we avoid storing a new default function and handle
	-- this case in runK.
	local id = effectNameToId[name]
	if id == nil then
		_nid += 1
		id = _nid
		idToEffectName[id] = name
		effectNameToId[name] = id
	end
	return { tag = TAG_EFF, eid = id, payload = payload, k = nil } :: EffNode
end

local function hOp(name: string, payload: any?, s: SubComputations?, k: ContFunc?): EffNode
	local hId = hoEffectNameToId[name]
	if not hId then
		_hoNid -= 1
		hId = _hoNid
		hoIdToEffectName[-hId] = name
		hoEffectNameToId[name] = -hId
	end
	if s then
		return { tag = TAG_EFF, eid = hId, payload = payload, k = k, s = s } :: EffNode
	end
	return { tag = TAG_EFF, eid = hId, payload = payload, k = k } :: EffNode
end

local function concatK(a: Cont, b: Cont): Cont
	if a == nil then
		return b
	end
	if b == nil then
		return a
	end
	return { l = a, r = b } :: ContPair
end

local function runK(k: Cont, v: any): Node
	if k == nil then
		return { tag = TAG_RET, val = v } :: RetNode
	end
	if type(k) == "function" then
		return (k :: ContFunc)(v)
	end

	local stk = _acq()
	local sp: i32 = 0
	local cur: Cont = k
	local val: any = v

	while true do
		while type(cur) == "table" do
			sp += 1
			stk[sp] = (cur :: ContPair).r
			cur = (cur :: ContPair).l
		end

		local nd: Node
		if type(cur) == "function" then
			nd = (cur :: ContFunc)(val)
		else
			nd = { tag = TAG_RET, val = val } :: RetNode
		end

		if nd.tag == TAG_RET then
			if sp == 0 then
				_rel(stk)
				return nd
			end
			val = (nd :: RetNode).val
			cur = stk[sp]
			stk[sp] = nil
			sp -= 1
		else
			local e: EffNode = nd :: EffNode
			local acc: Cont = e.k
			while sp > 0 do
				acc = concatK(acc, stk[sp])
				stk[sp] = nil
				sp -= 1
			end
			_rel(stk)
			return mkEff(e.eid, e.payload, acc, e.s) :: Node
		end
	end
end

-- Strictly speaking, this is closer to a monadic bind
local function seq(node: Node, k: ContFunc): Node
	if node.tag == TAG_RET then
		return k((node :: RetNode).val)
	end
	local e: EffNode = node :: EffNode
	return mkEff(e.eid, e.payload, concatK(e.k, k), e.s) :: Node
end

local function handles(name: string)
	return function(target: Handler, propertyKey: string, descriptor)
		local id = effectNameToId[name]
		if id == nil then
			_nid += 1
			id = _nid
			idToEffectName[id] = name
			effectNameToId[name] = id
		end
		target.methods = target.methods or {}
		target.methods[id] = target[propertyKey]
	end
end

local function elaborates(name: string)
	return function(target: Elaborator, propertyKey: string, descriptor)
		local id = hoEffectNameToId[name]
		if id == nil then
			_hoNid -= 1
			id = _hoNid
			hoIdToEffectName[-id] = name
			hoEffectNameToId[name] = id
		end
		target.methods = target.methods or {}
		target.methods[id] = target[propertyKey]
	end
end

local function withHandler(handler: Handler): { run: (Node) -> Node }
	local methods = handler.methods
	local rf = handler["return"]

	local function step(n: Node, pass: boolean): Node
		while true do
			if n.tag == TAG_RET then
				if not pass and rf then
					return (rf :: (Handler, any) -> Node)(handler, (n :: RetNode).val)
				end
				return n
			end

			local e: EffNode = n :: EffNode
			local m = methods[e.eid]
			local k: Cont = e.k

			if m then
				local resume = function(v: any): Node
					return step(runK(k, v), false)
				end
				local result: Node = m(handler, e.payload, resume)
				if result.tag == TAG_EFF then
					n = result
					pass = true
					continue
				end
				return result
			end

			local resume = function(v: any): Node
				return step(runK(k, v), pass)
			end

			return mkEff(e.eid, e.payload, resume, e.s) :: Node
		end
	end

	return {
		run = function(node: Node): Node
			return step(node, false)
		end,
	}
end

local function withElaborator(elaborator: Elaborator): { run: (Node) -> Node }
	local methods = elaborator.methods

	local function elaborate(node: Node): Node
		while true do
			if node.tag == TAG_RET then
				return node
			end

			local e: EffNode = node :: EffNode
			local m = methods[e.eid]
			local k: Cont = e.k
			local s: SubComputations? = e.s
			if s then
				for i = 1, #s do
					local subs = e.s :: SubComputations
					subs[i] = elaborate(subs[i])
				end
			end
			local resume = function(v: any): Node
				return elaborate(runK(k, v))
			end

			if m then
				local result: Node = m(elaborator, e.payload, s, resume)
				if result.tag == TAG_EFF then
					node = result
					continue
				end
				return result
			end

			-- Not an effect we elaborate; forward with elaborated continuation
			return mkEff(e.eid, e.payload, resume, e.s) :: Node
		end
	end

	return { run = elaborate }
end

local function _runBranch(initial: Node, ctx: _BranchCtx): ()
	local cur: Node = initial
	while true do
		if cur.tag == TAG_RET then
			local res = ctx.results
			res[#res + 1] = (cur :: RetNode).val
			local pending = ctx.pending - 1
			ctx.pending = pending
			if pending == 0 then
				ctx.resolve(res)
			end
			return
		end

		local e: EffNode = cur :: EffNode
		local eid: i32 = e.eid
		local payload: any = e.payload
		local k: Cont = e.k

		if eid == EID_IO then
			cur = runK(k, payload())
		elseif eid == EID_FAIL then
			-- Branch is pruned; just retire this fiber.
			local pending = ctx.pending - 1
			ctx.pending = pending
			if pending == 0 then
				ctx.resolve(ctx.results)
			end
			return
		elseif eid == EID_ASYNC then
			-- Suspend this fiber until the promise resolves.
			payload():andThen(function(v: any)
				_runBranch(runK(k, v), ctx)
			end, ctx.reject)
			return -- fiber is now suspended; do NOT decrement pending here
		elseif eid == EID_NDET then
			if payload == nil then
				-- Boolean branch: current fiber → true, spawn false branch.
				ctx.pending += 1
				_runBranch(runK(k, false), ctx)
				cur = runK(k, true)
			elseif type(payload) == "table" then
				local arr: { any } = payload :: { any }
				local n: i32 = #arr
				if n == 0 then
					local pending = ctx.pending - 1
					ctx.pending = pending
					if pending == 0 then
						ctx.resolve(ctx.results)
					end
					return
				end
				-- Current fiber → arr[1]; spawn arr[2..n].
				ctx.pending += (n - 1)
				for i = n, 2, -1 do
					_runBranch(runK(k, arr[i]), ctx)
				end
				cur = runK(k, arr[1])
			elseif type(payload) == "number" then
				local cnt: i32 = payload :: i32
				if cnt == 0 then
					local pending = ctx.pending - 1
					ctx.pending = pending
					if pending == 0 then
						ctx.resolve(ctx.results)
					end
					return
				end
				ctx.pending += (cnt - 1)
				for i = cnt, 2, -1 do
					_runBranch(runK(k, i), ctx)
				end
				cur = runK(k, 1)
			else
				ctx.reject("ndet: invalid parameter, expected nil, number, or table")
				return
			end
		else
			ctx.reject(`Unhandled effect: ${getEffectName(eid)}`)
			return
		end
	end
end

local function run(node: Node): any
	local cur: Node = node
	local results: { any }? = nil
	local resp = 0
	local br: { any }? = nil
	local bp = 0

	while true do
		if cur.tag == TAG_RET then
			if results == nil then
				return (cur :: RetNode).val
			end
			local res = results :: { any }
			res[resp + 1] = (cur :: RetNode).val
			resp += 1
			if bp == 0 then
				return results
			end
			local brStack = br :: { any }
			cur = brStack[bp] :: Node
			brStack[bp] = nil
			bp -= 1
			continue
		end

		local e: EffNode = cur :: EffNode
		local eid: i32 = e.eid
		local p: any = e.payload
		local k: Cont = e.k

		if eid == EID_IO then
			cur = runK(k, p())
		elseif eid == EID_FAIL then
			-- Fail prunes branches after ndet is encountered
			if results == nil then
				error("Computation failed")
			end
			if bp == 0 then
				return results
			end
			local brStack = br :: { any }
			cur = brStack[bp]
			brStack[bp] = nil
			bp -= 1
		elseif eid == EID_ASYNC then
			if results == nil then
				return p():andThen(function(v: any)
					return run(runK(k, v))
				end)
			end

			local innerPromise = p()

			local PromiseLib = getmetatable(innerPromise)
			local brStack = br :: { any }

			local ctx: _BranchCtx = {
				results = results :: { any }, -- pre-populated with any sync results
				pending = bp + 1,
				resolve = nil :: any,
				reject = nil :: any,
			}

			local outerPromise = PromiseLib.new(function(resolve: any, reject: any)
				ctx.resolve = resolve
				ctx.reject = reject
			end)

			innerPromise:andThen(function(v: any)
				_runBranch(runK(k, v), ctx)
			end, ctx.reject)

			for i = 1, bp do
				_runBranch(brStack[i] :: Node, ctx)
				brStack[i] = nil
			end

			return outerPromise
		elseif eid == EID_NDET then
			if results == nil then
				results = {} :: { any }
				br = {} :: { any }
			end
			local brStack = br :: { any }

			if p == nil then
				bp += 1
				brStack[bp] = runK(k, false)
				cur = runK(k, true)
			elseif type(p) == "table" then
				local arr: { any } = p :: { any }
				local n: i32 = #arr
				if n == 0 then
					if bp == 0 then
						return results
					end
					cur = brStack[bp] :: Node
					brStack[bp] = nil
					bp -= 1
				else
					for i = n, 2, -1 do
						bp += 1
						brStack[bp] = runK(k, arr[i])
					end
					cur = runK(k, arr[1])
				end
			elseif type(p) == "number" then
				local cnt: i32 = p :: i32
				for i = cnt, 2, -1 do
					bp += 1
					brStack[bp] = runK(k, i)
				end
				cur = runK(k, 1)
			else
				error("ndet: invalid parameter, expected nil, number, or table")
			end
		elseif eid == EID_APPLY then
			local s = e.s
			local subresults = {}
			if s and #s > 0 then
				for i = 1, #s do
					subresults[i] = run(s[i])
				end
			end
			cur = runK(k, p(table.unpack(subresults)))
		else
			error(`Unhandled effect: ${getEffectName(eid)}`)
		end
	end
end

local function do_(genFn: () -> any): Node
	local iter = genFn()
	local function step(v: any): Node
		local r = iter.next(v)
		if r.done then
			return ret(r.value)
		end
		return seq(r.value, step)
	end
	return step(nil)
end

local function pipe(initial: any, ...: (any) -> any): any
	local fns = { ... }
	local acc = initial
	for _, fn in fns do
		acc = (fn :: any)(acc)
	end
	return acc
end

return table_freeze({
	-- Node constructors
	ret = ret,
	op = op,
	hOp = hOp,

	-- sequencing
	seq = seq,

	-- Handlers/Elaborators
	withHandler = withHandler,
	withElaborator = withElaborator,

	-- Decorators
	handles = handles,
	elaborates = elaborates,

	-- Runtime
	run = run,

	-- Utilities
	do_ = do_,
	pipe = pipe,
})
