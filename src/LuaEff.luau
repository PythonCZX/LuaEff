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
export type EffNode  = { tag: i32, eid: i32, payload: any, k: Cont, s: any? }
export type Node     = RetNode | EffNode

-- Recursive types cannot have parameters.
--     type ContFunc<T> = (T) -> Node 
export type ContFunc        = (any) -> Node
export type ContPair        = { l: Cont, r: Cont }
export type Cont            = ContFunc | ContPair | nil
export type SubComputations = { [string]: Node }
export type Effect          = { id: i32, name: string }

local HandlerBuilder = {}
HandlerBuilder.__index = HandlerBuilder
export type HandlerBuilder = typeof(setmetatable(
	{} :: {
		_methods: { [i32]: (any, (any) -> Node) -> Node },
    _ret: ((any) -> Node)?
	},
	HandlerBuilder
))

local ElaboratorBuilder = {}
ElaboratorBuilder.__index = ElaboratorBuilder
export type ElaboratorBuilder = typeof(setmetatable(
	{} :: {
    _methods: { [i32]: (any, any, (any) -> Node, (Node) -> Node) -> Node },
    _introduces: { Effect }
	},
	ElaboratorBuilder
))

local TAG_RET        = 0
local TAG_EFF        = 1
local table_create   = table.create
local table_freeze   = table.freeze

local EID_IO:    i32 = 1
local EID_FAIL:  i32 = 2
local EID_ASYNC: i32 = 3
local EID_NDET:  i32 = 4

local idToEffectName = {
  "io",
  "fail",
  "async",
  "ndet",
}
local hoIdToEffectName = {}

local _nid: i32 = 4
local _hoNid: i32 = 0

local function newEffect(name: string): Effect
	_nid += 1
  idToEffectName[_nid] = name
	return table_freeze({ id = _nid, name = name }) :: Effect
end

local function newHoEffect(name: string): Effect
	_hoNid -= 1
  hoIdToEffectName[-_hoNid] = name
	return table_freeze({ id = _hoNid, name = name }) :: Effect
end

local function getEffectName(id: i32): string
  if id > 0 then return idToEffectName[id] end
  return hoIdToEffectName[-id]
end

local IO: Effect    = table_freeze({ id = EID_IO,    name = "io"    }) :: Effect
local FAIL: Effect  = table_freeze({ id = EID_FAIL,  name = "fail"  }) :: Effect
local ASYNC: Effect = table_freeze({ id = EID_ASYNC, name = "async" }) :: Effect
local NDET: Effect  = table_freeze({ id = EID_NDET,  name = "ndet"  }) :: Effect

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

local function mkEff(eid: i32, p: any, k: Cont, s: SubComputations?): EffNode
	if s then
		return { tag = TAG_EFF, eid = eid, payload = p, k = k, s = s }
	end
	return { tag = TAG_EFF, eid = eid, payload = p, k = k }
end

local function op(effect: Effect, payload: any?, k: ContFunc?): Node
	return { tag = TAG_EFF, eid = effect.id, payload = payload, k = k } :: Node
end

local function hOp(effect: Effect, payload: any?, s: SubComputations?, k: ContFunc?): Node
	if s then
		return { tag = TAG_EFF, eid = effect.id, payload = payload, k = k, s = s } :: Node
	end
	return { tag = TAG_EFF, eid = effect.id, payload = payload, k = k } :: Node
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

function HandlerBuilder.new(): HandlerBuilder
	local self = setmetatable({
		_methods = {},
		_ret = nil,
	}, HandlerBuilder)
	return self :: HandlerBuilder
end

function HandlerBuilder.handle(self: HandlerBuilder, effect: Effect, handler: (any, (any) -> Node) -> Node)
	self._methods[effect.id] = handler
	return self
end

function HandlerBuilder.mapReturn(self: HandlerBuilder, map: (any) -> Node)
	self._ret = map
	return self
end

function HandlerBuilder.build(self: HandlerBuilder): { handle: (Node) -> Node }
	local methods = self._methods
	local rf = self._ret

	local function step(n: Node, pass: boolean): Node
		while true do
			if n.tag == TAG_RET then
				if not pass and rf then
					return (rf :: (any) -> Node)((n :: RetNode).val)
				end
				return n
			end

			local e: EffNode = n :: EffNode
			local m = methods[e.eid]
			local k: Cont = e.k

			local resume = function(v: any): Node
				return step(runK(k, v), false)
			end

			if m then
				local result: Node = m(e.payload, resume)
				if result.tag == TAG_EFF then
					n = result
					pass = true
					continue
				end
				return result
			end

			return mkEff(e.eid, e.payload, resume, e.s) :: Node
		end
	end

	return {
		handle = function(node: Node): Node
			return step(node, false)
		end,
	}
end

function ElaboratorBuilder.new(...)
	local self = setmetatable({
		_methods = {},
		_introduces = {},
	}, ElaboratorBuilder)
	return self :: ElaboratorBuilder
end

function ElaboratorBuilder.elaborate(
	self: ElaboratorBuilder,
	effect: Effect,
	elaborator: (any, any, (any) -> Node, (Node) -> Node) -> Node
)
	self._methods[effect.id] = elaborator
	return self
end

function ElaboratorBuilder.introduces(self: ElaboratorBuilder, effects: { Effect })
	self._introduces = effects
	return self
end

function ElaboratorBuilder.build(self: ElaboratorBuilder): { elaborate: (Node) -> Node }
	local methods = self._methods

	local function elaborate(node: Node): Node
		if node.tag == TAG_RET then
			return node
		end

		local e: EffNode = node :: EffNode
		local m = methods[e.eid]
		local k: Cont = e.k
		local resume = function(v: any): Node
			return elaborate(runK(k, v))
		end

		if m then
			local result: Node = m(e.payload, e.s, resume, elaborate)
			return elaborate(result)
		end

		-- Not an effect we elaborate; forward with elaborated continuation
		return mkEff(e.eid, e.payload, resume, e.s) :: Node
	end

	return { elaborate = elaborate }
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
			return p():andThen(function(v: any)
				return run(runK(k, v))
			end)
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
		else
			error(`Unhandled effect: ${getEffectName(eid)}`)
		end
	end
end

local function do_(genFn: () -> any): Node
	local iter = genFn()
	local function step(v: any): Node
		local r = iter:next(v)
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
	-- Token constructors
	newEffect = newEffect,
	newHoEffect = newHoEffect,

	-- Built-in effect tokens
	IO = IO,
	FAIL = FAIL,
	ASYNC = ASYNC,
	NDET = NDET,

	-- Node constructors
	ret = ret,
	op = op,
	hOp = hOp,
	mkEff = mkEff,

	-- sequencing
	seq = seq,

	-- Builders
	Handler = HandlerBuilder,
	Elaborator = ElaboratorBuilder,

	-- Runtime
	run = run,

	-- Utilities
	do_ = do_,
	pipe = pipe,
})
