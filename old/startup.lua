--[[
	
	Auto routing and auto-craft planner script for computer craft
	Author: Yibo Cao
	
]]

-------------
--utilities--
-------------

local class = (loadfile "middleclass.lua")()

local function unpack(tab)
	if table.unpack ~= nil then return table.unpack(tab) end
	return _G.unpack(tab)
end

local function pack(...)
	return {...}
end

local function bind(f, ...)
	local args = {...}
	return function(...)
		local tmp, newArgs = {}, {...}
		for k, v in pairs(args) do tmp[k] = v end
		for k, v in pairs(newArgs) do table.insert(tmp, v) end
		return f(unpack(tmp))
	end
end

--------
--item--
--------

local item = class("item")
item.initialize = function(self) end
item.equals = function(self, other) error("pure") end

------------
--routable--
------------

local routable = class("routable")
routable.initialize = function(self) end
--onDone = function(qtyDone)
routable.route = function(self, item, qty, dir, onDone) error("pure") end
--onUnexpected = function(item, qty)
routable.setUnexpectedHandler = function(self, onUnexpected) error("pure") end

------------------
--routingManager--
------------------

local routingManager = class("routingManager")
routingManager.initialize = function(self)
	--{name = routable}
	self.nodes = {}
	--{srcName = {dir = {dstName = dstName, cost = cost}}}
	self.dirMap = {}
	self.onUnexpected = function(nodeName, item, qty)
		print("unhandled item on " .. nodeName .. ", " .. tostring(item) .. "*" .. qty)
	end
end
routingManager.addNode = function(self, name, node)
	self.nodes[name] = node
	self.dirMap[name] = {}
	node:setUnexpectedHandler(function(item, qty)
		self.onUnexpected(name, item, qty)
	end)
end
--cost is optional (default: 1)
routingManager.addRoute = function(self, srcName, dir, dstName, cost)
	if cost == nil then cost = 1 end
	self.dirMap[srcName][dir] = {dstName = dstName, cost = cost}
end
--onDone = function(qtyDone)
--onDone is optional (default: print a message)
routingManager.route = function(self, item, qty, srcName, dstName, dir, onDone)
	if onDone == nil then
		local accum = {0}
		onDone = function(qtyDone)
			accum[1] = accum[1] + qtyDone
			print("routing done, " .. srcName .. "->" .. dstName .. ", " .. dir .. ", " .. tostring(item) .. "*" .. accum[1] .. "/" .. qty)
		end
	end
	
	if srcName == dstName then
		self.nodes[srcName]:route(item, qty, dir, onDone)
		return
	end
	
	--{{marked = marked, dist = dist, prev = {prevName = prevName, prevDir = prevDir}}}
	local info = {}
	--{nodeName = true}
	local pending = {}
	for k, v in pairs(self.dirMap) do
		info[k] = {}
		info[k].marked = false
		info[k].dist = math.huge
	end
	info[srcName].dist = 0
	pending[srcName] = true
	while true do
		local nowNodeName = nil
		local nowNodeDist = math.huge
		for k, v in pairs(pending) do
			if info[k].dist < nowNodeDist then
				nowNodeName = k
				nowNodeDist = info[k].dist
			end
		end
		if nowNodeName == nil then error ("destination unreachable(" .. srcName .. ", " .. dstName .. ")") end
		if nowNodeName == dstName then break end
		pending[nowNodeName] = nil
		info[nowNodeName].marked = true
		for k, v in pairs(self.dirMap[nowNodeName]) do
			local nextNodeName = v.dstName
			local nextNodeInfo = info[nextNodeName]
			if not nextNodeInfo.marked then
				local newDist = nowNodeDist + v.cost
				if newDist < info[nextNodeName].dist then
					info[nextNodeName].dist = newDist
					info[nextNodeName].prev = {prevName = nowNodeName, prevDir = k}
				end
				pending[nextNodeName] = true
			end
		end
	end
	
	--callback chaining
	local now = dstName
	local lastDir = dir
	while info[now].prev ~= nil do
		onDone = bind(function(closure, qtyDone)
			self.nodes[closure.now]:route(item, qtyDone, closure.dir, closure.onDone)
		end, {now = now, onDone = onDone, dir = lastDir})
		lastDir = info[now].prev.prevDir
		now = info[now].prev.prevName
	end
	
	if now ~= srcName then error("unreachable") end
	
	self.nodes[now]:route(item, qty, lastDir, onDone)
end
--handler = function(nodeName, item, qty)
routingManager.setUnexpectedHandler = function(self, handler)
	self.onUnexpected = handler
end

------------
--provider--
------------

local provider = class("provider")
provider.initialize = function(self) end
--return number of available items
provider.canProvide = function(self, item, qty) error("pure") end
--reserve an item. return value = {cancel = function(), perform = function(dstNodeName, dir, onDone)}
--	onDone = function(qtyDone)
provider.reserve = function(self, item, qty) error("pure") end
--return priority of this provider for an item
provider.priority = function(self, item, qty) error("pure") end

--------
--sink--
--------

local sink = class("sink")
sink.initialize = function(self) end
--return number of available spaces for item
sink.canSink = function(self, item, qty) error("pure") end
--must success. return value = {dstNodeName = dstNodeName, dir = dir, qty = qty, onDone = onDone}
sink.sink = function(self, item, qty) error("pure") end
--return priority of this sink for an item
sink.priority = function(self, item, qty) error("pure") end

-----------
--crafter--
-----------

local crafter = class("crafter")
crafter.initialize = function(self) end
--return result item
crafter.getResult = function(self) error("pure") end
--return number of result items
crafter.numResult = function(self) error("pure") end
--return list of materials required
--return value = {{item = item, dstNodeName = dstNodeName, dir = dir}}
crafter.getMaterials = function(self) error("pure") end
--perform the crafting
--return = {onMaterialDelivered = onMaterialDelivered, onAllMaterialsDelivered = onAllMaterialsDelivered}
--onMaterialDelivered = function(materialIndex)
--onAllMaterialsDelivered = function(destinations)
--	destinations = {{dstNodeName = dstNodeName, dir = dir, qty = qty, onDone}}
--		onDone = function(qtyDone)
--
--if destinations don't designate all result items, sink the remaining items
crafter.craft = function(self) error("pure") end
--return priority of this crafter. (usually inversely-related to the current jobs)
crafter.priority = function(self) error("pure") end

------------------
--factoryManager--
------------------

local factoryManager = class('factoryManager')
factoryManager.initialize = function(self)
	self.rm = routingManager:new()
	--{provider}
	self.providers = {}
	--{sink}
	self.sinks = {}
	--{crafter}
	self.crafters = {}
	
	self.rm:setUnexpectedHandler(function(nodeName, item, qty)
		self:onUnexpected(nodeName, item, qty)
	end)
end
factoryManager.addNode = function(self, name, node)
	self.rm:addNode(name, node)
end
factoryManager.addRoute = function(self, srcName, dir, dstName, cost)
	self.rm:addRoute(srcName, dir, dstName, cost)
end
factoryManager.route = function(self, item, qty, srcName, dstName, dir, onDone)
	self.rm:route(item, qty, srcName, dstName, dir, onDone)
end
factoryManager.addProvider = function(self, provider)
	table.insert(self.providers, provider)
end
factoryManager.addSink = function(self, sink)
	table.insert(self.sinks, sink)
end
factoryManager.addCrafter = function(self, crafter)
	table.insert(self.crafters, crafter)
end
factoryManager.onUnexpected = function(self, nodeName, item, qty)
	self:sinkItem(nodeName, item, qty, function(qty) end)
end
--onDone = function(qtyDone)
factoryManager.sinkItem = function(self, nodeName, item, qty, onDone)
	while qty > 0 do
		--{sink = sink, priority = priority, canSink = canSink}
		local bestSink = nil
		for k, v in pairs(self.sinks) do
			local canSink = v:canSink(item, qty)
			if canSink > 0 then
				local priority = v:priority(item, qty)
				if bestSink == nil or priority > bestSink.priority then
					bestSink = {sink = v, priority = priority, canSink = canSink}
				end
			end
		end
		if bestSink == nil then
			error("no available sink for " .. tostring(item) .. "*" .. qty)
		end
		local result = bestSink.sink:sink(item, bestSink.canSink)
		self:route(item, result.qty, nodeName, result.dstNodeName, result.dir, result.onDone)
		qty = qty - bestSink.canSink
	end
end
--return value = {crafter}
factoryManager.sortCrafter = function(self, item)
	local crafters = {}
	for k, v in pairs(self.crafters) do
		if v:getResult():equals(item) then
			table.insert(crafters, v)
		end
	end
	table.sort(crafters, function(x, y)
		return x:priority() > y:priority()
	end)
	return crafters
end
--return value = {provider = provider, canProvide = canProvide, priority = priority} or nil
factoryManager.searchProvider = function(self, item, qty)
	local bestProvider = nil
	for k, v in pairs(self.providers) do
		local canProvide = v:canProvide(item, qty)
		if canProvide > 0 then
			local priority = v:priority(item, qty)
			if bestProvider == nil or priority > bestProvider.priority then
				bestProvider = {priority = priority, provider = v, canProvide = canProvide}
			end
		end
	end
	return bestProvider
end
--return value = {cancel = function(), perform = function(onDone), qtyAvail = qtyAvail}
--	onDone = function(qtyDone)
factoryManager.planCrafting = function(self, item, qty, dstNodeName, dir)
	local root = {}
	local function buildNode(node, parent, item, qty, dstNodeName, dir)
		node.parent = parent
		node.item = item
		node.qty = qty
		node.dstNodeName = dstNodeName
		node.dir = dir
		node.qtyAvail = 0
		node.children = {}
		node.extraQty = 0
		--{usedEntry} (keySet)
		--usedEntry = {qty = qty, dstNodeName = dstNodeName, dir = dir, onDelivered = onDelivered}
		node.extraUsedList = {}
		--{{node = node, usedEntry = usedEntry}}
		node.usedExtras = {}
		node.extrasRemaining = function()
			local qty = node.extraQty
			for k, v in pairs(node.extraUsedList) do
				qty = qty - k.qty
			end
			return qty
		end
		--{reserveEntry}
		--reserveEntry = {cancel = function(), perform = function(onDone), qtyUsed = qtyUsed}
		node.usedProviders = {}
		--{{crafter = crafter, children = {child}, numNotExtra = numNotExtra, numExtra = numExtra, assignedExtras = {{qty, dstNodeName, dir, onDelivered}}, numAssignedExtras = numAssignedExtras}}
		node.usedCrafters = {}
		
		node.cancel = function()
			while #node.children > 0 do
				local now = node.children[#node.children]
				now.cancel()
				table.remove(node.children)
			end
			
			while #node.usedProviders > 0 do
				local now = node.usedProviders[#node.usedProviders]
				now.cancel()
				table.remove(node.usedProviders)
			end
			
			while #node.usedExtras > 0 do
				local now = node.usedExtras[#node.usedExtras]
				if now.node.extraUsedList[now] then
					now.node.extraUsedList[now] = nil
				else
					error("missing usedEntry")
				end
				table.remove(node.usedExtras)
			end
			
			if node.parent ~= nil then
				for k, v in pairs(node.parent.children) do
					if v == node then
						table.remove(node.parent.children, k)
						return
					end
				end
			end
			
			error("node not found in parent's children list")
		end
		
		node.perform = function(onDone)
			node.onDone = onDone
			
			--assign extras
			local function allocExtraFor(dstNodeName, dir, onDelivered, qty)
				for k, v in pairs(node.usedCrafters) do
					local avail = v.numExtra - v.numAssignedExtras
					if avail > 0 then
						local need = math.min(avail, qty)
						v.numAssignedExtras = v.numAssignedExtras + need
						table.insert(v.assignedExtras, {qty = need, dstNodeName = dstNodeName, dir = dir, onDelivered = onDelivered})
						return need
					end
				end
				error("no more extra to allocate")
			end
			
			for k, v in pairs(node.extraUsedList) do
				local remaining = k.qty
				while remaining > 0 do
					remaining = remaining - allocExtraFor(k.dstNodeName, k.dir, k.onDelivered, remaining)
				end
			end
			
			--perform crafting
			for k, v in pairs(node.usedCrafters) do
				local destinations = {}
				table.insert(destinations, {dstNodeName = node.dstNodeName, dir = node.dir, qty = v.numNotExtra, onDone = onDone})
				for k1, v1 in pairs(v.assignedExtras) do
					table.insert(destinations, {dstNodeName = v1.dstNodeName, dir = v1.dir, qty = v1.qty, onDone = v1.onDelivered})
				end
				local cbs = v.crafter:craft()
				local materialsRemaining = {#v.children}
				for k1, v1 in pairs(v.children) do
					v1.perform(bind(function(k1, cbs, materialsRemaining, destinations, qtyDone)
						if qtyDone ~= 1 then
							error("qtyDone for crafting material isn't 1")
						end
						cbs.onMaterialDelivered(k1)
						materialsRemaining[1] = materialsRemaining[1] - 1
						if materialsRemaining[1] == 0 then
							cbs.onAllMaterialsDelivered(destinations)
						end
					end, k1, cbs, materialsRemaining, destinations))
				end
			end
			
			--perform providing
			for k, v in pairs(node.usedProviders) do
				v.perform(node.dstNodeName, node.dir, onDone)
			end
		end
		
		if parent ~= nil then
			table.insert(parent.children, node)
		end
		
		do --make sure no cycles
			local now = parent
			while now ~= nil do
				if now.item:equals(item) then
					return
				end
				now = now.parent
			end
		end
		
		do --check extras
			local stack = {root}
			while node.qtyAvail < node.qty and #stack > 0 do
				local now = stack[#stack]
				table.remove(stack)
				if now.item:equals(node.item) then
					local availExtra = now:extrasRemaining()
					if availExtra > 0 then
						local needExtra = math.min(availExtra, node.qty - node.qtyAvail)
						node.qtyAvail = node.qtyAvail + needExtra
						local newEntry = {qty = needExtra, dstNodeName = node.dstNodeName, dir = node.dir}
						table.insert(node.usedExtras, {node = now, usedEntry = newEntry})
						now.extraUsedList[newEntry] = true
						newEntry.onDelivered = function(qtyDone)
							node.onDone(qtyDone)
						end
					end
				end
				for k, v in pairs(now.children) do
					table.insert(stack, v)
				end
			end
		end
		
		do --check providers
			while node.qtyAvail < node.qty do
				local providerFound = self:searchProvider(node.item, node.qty)
				if providerFound == nil then
					break
				end
				local needProvide = math.min(providerFound.canProvide, node.qty - node.qtyAvail)
				node.qtyAvail = node.qtyAvail + needProvide
				local rst = providerFound.provider:reserve(node.item, needProvide)
				local reserveEntry = {cancel = rst.cancel, perform = rst.perform, qtyUsed = needProvide}
				table.insert(node.usedProviders, reserveEntry)
			end
		end
		
		do --check crafters
			local cont = true
			while node.qtyAvail < node.qty and cont do
				cont = false
				local crafters = self:sortCrafter(node.item)
				if #crafters == 0 then
					break
				end
				for k, v in pairs(crafters) do
					if node.qtyAvail == node.qty then
						break
					end
					local materials = v:getMaterials()
					local tmpChildren = {}
					for k, v in pairs(materials) do
						tmpChildren[k] = {}
						buildNode(tmpChildren[k], node, v.item, 1, v.dstNodeName, v.dir)
						if tmpChildren[k].qtyAvail == 0 then
							for k, v in pairs(tmpChildren) do
								v.cancel()
							end
							tmpChildren = nil
							break
						end
					end
					if tmpChildren ~= nil then
						cont = true
						local avail = v:numResult()
						local need = math.min(node.qty - node.qtyAvail, avail)
						node.qtyAvail = node.qtyAvail + need
						local surplus = avail - need
						if surplus > 0 then
							node.extraQty = node.extraQty + surplus
						end
						table.insert(node.usedCrafters, {crafter = v, children = tmpChildren, numNotExtra = need, numExtra = surplus, assignedExtras = {}, numAssignedExtras = 0})
					end
				end
			end
		end
	end
	buildNode(root, nil, item, qty, dstNodeName, dir)
	return root
end

----------------------------
--computer craft utilities--
----------------------------

local function pc(...)
	local rst = pack(pcall(peripheral.call, unpack({...})))
	if rst[1] == false then return nil end
	return select(2, unpack(rst))
end

local function pushItemHelper(peri, fromSlot, dir, qty)
	local rst
	local pos = string.find(dir, "%.")
	if pos == nil then
		rst = pc(peri, "pushItem", dir, fromSlot, qty)
	else
		local newDir = string.sub(dir, 1, pos - 1)
		local slot = string.sub(dir, pos + 1)
		rst = pc(peri, "pushItem", newDir, fromSlot, qty, slot)
	end
	if rst == nil or rst < 0 then
		return 0
	else
		return rst
	end
end

----------------------------
--minecraft item (id, dmg)--
----------------------------

local mcItem = class('mcItem', item)
mcItem.initialize = function(self, id, dmg)
	self.id = id
	self.dmg = dmg
end
mcItem.equals = function(self, other)
	return self.id == other.id and self.dmg == other.dmg
end
mcItem.__tostring = function(self)
	return self.id .. ":" .. self.dmg
end

-----------------------
--simple routing node--
-----------------------

local nodeSimple = class('nodeSimple', routable)
nodeSimple.initialize = function(self, peri)
	self.peri = peri
	--{{item = item, qty = qty, dir = dir, onDone = onDone}}
	self.pending = {}
end
nodeSimple.route = function(self, item, qty, dir, onDone)
	table.insert(self.pending, {item = item, qty = qty, dir = dir, onDone = onDone})
end
nodeSimple.setUnexpectedHandler = function(self, handler)
	self.unexpect = handler
end
nodeSimple.tick = function(self, fm)
	local stks = pc(self.peri, "getAllStacks")
	if stks == nil then
		print("getAllStacks failed for " .. self.peri)
		return
	end
	local newStks = {}
	for k, v in pairs(stks) do
		local stk = v.basic()
		stk.item = mcItem:new(stk.id, stk.dmg)
		newStks[k] = stk
	end
	stks = newStks
	
	local idxToRemove = {}
	for k, v in pairs(self.pending) do
		for k1, v1 in pairs(stks) do
			if v.qty == 0 then
				break
			end
			if v1.item:equals(v.item) and v1.qty > 0 then
				local need = math.min(v1.qty, v.qty)
				local rst = pushItemHelper(self.peri, k1, v.dir, need)
				if rst == 0 then
					print("pushItem failed for " .. self.peri)
					v1.qty = 0
				elseif rst > need then
					error "pushItem pushed more item than required"
				else
					if (rst < need) then
						print("item stuck at " .. self.peri)
						v1.qty = 0
					else
						v1.qty = v1.qty - rst
					end
					v.qty = v.qty - rst
					v.onDone(rst)
				end
			end
		end
		if v.qty == 0 then
			table.insert(idxToRemove, k)
		end
	end
	
	local offset = 0
	for k, v in pairs(idxToRemove) do
		table.remove(self.pending, v + offset)
		offset = offset - 1
	end
	
	for k, v in pairs(stks) do
		if v.qty > 0 then
			self.unexpect(v.item, v.qty)
		end
	end
end

------------------
--barrel storage--
------------------

local barrel = class("barrel")
barrel.initialize = function(self, peri, item, maxQty, inputNode, inputDir, outputNode, outputDir)
	self.peri = peri
	self.item = item
	self.inputNode = inputNode
	self.inputDir = inputDir
	self.outputNode = outputNode
	self.outputDir = outputDir
	self.qtyContain = 0
	self.qtyToSink = 0
	self.qtyReserved = 0
	self.maxQty = maxQty
	self.providerTasks = {}
end
local barrelProvider = class("barrelProvider", provider)
barrelProvider.initialize = function(self, barrel, priority)
	self.barrel = barrel
	self.numPriority = priority
end
barrelProvider.canProvide = function(self, item, qty)
	if item:equals(self.barrel.item) then
		return self.barrel.qtyContain + self.barrel.qtyToSink - self.barrel.qtyReserved
	else
		return 0
	end
end
barrelProvider.reserve = function(self, item, qty)
	self.barrel.qtyReserved = self.barrel.qtyReserved + qty
	return {
		cancel = function()
			self.barrel.qtyReserved = self.barrel.qtyReserved - qty
		end,
		perform = function(dstNodeName, dir, onDone)
			table.insert(self.barrel.providerTasks, {dstNodeName = dstNodeName, dir = dir, onDone = onDone, qty = qty})
		end
	}
end
barrelProvider.priority = function(self, item, qty)
	return self.numPriority
end
local barrelSink = class("barrelSink", sink)
barrelSink.initialize = function(self, barrel, priority)
	self.barrel = barrel
	self.numPriority = priority
end
barrelSink.canSink = function(self, item, qty)
	if item:equals(self.barrel.item) then
		local used = self.barrel.qtyContain + self.barrel.qtyToSink
		local avail = self.barrel.maxQty - used
		if avail < 0 then
			error("barrel for " .. tostring(self.barrel.item) .. " may contain more item than maxQty")
		end
		return math.min(avail, qty)
	else
		return 0
	end
end
barrelSink.sink = function(self, item, qty)
	self.barrel.qtyToSink = self.barrel.qtyToSink + qty
	return {
		dstNodeName = self.barrel.inputNode,
		dir = self.barrel.inputDir,
		qty = qty,
		onDone = function(qtyDone)
			self.barrel.qtyToSink = self.barrel.qtyToSink - qtyDone 
			self.barrel.qtyContain = self.barrel.qtyContain + qtyDone
		end
	}
end
barrelSink.priority = function(self, item, qty)
	return self.numPriority
end
barrel.tick = function(self, fm)
	local itms = pc(self.peri, "getStoredItems")
	if itms == nil then
		print("getStoredItems failed for " .. self.peri)
		return
	end
	local theItem = mcItem:new(itms.id, itms.dmg)
	if not theItem:equals(self.item) then
		print("invalid item in barrel for " .. tostring(self.item) .. ": " .. tostring(theItem))
		return
	end
	self.qtyContain = itms.qty
	
	if self.qtyContain + self.qtyToSink < self.qtyReserved then
		print("barrel for " .. tostring(self.item) .. " overdraft")
	end
	local idxToRemove = {}
	for k, v in pairs(self.providerTasks) do
		if self.qtyContain == 0 then break end
		if v.qty > 0 then
			local canTake = math.min(self.qtyContain, v.qty)
			local rst = pushItemHelper(self.peri, 2, self.outputDir, canTake)
			if rst == 0 then
				print("pushItem failed for barrel for " .. tostring(self.item))
			else
				if rst < canTake then
					print("item stuck at barrel for " .. tostring(self.item))
				end
				self.qtyContain = self.qtyContain - rst
				self.qtyReserved = self.qtyReserved - rst
				v.qty = v.qty - rst
				fm:route(self.item, rst, self.outputNode, v.dstNodeName, v.dir, v.onDone)
			end
		end
		if v.qty == 0 then
			table.insert(idxToRemove, k)
		end
	end
	
	local offset = 0
	for k, v in pairs(idxToRemove) do
		table.remove(self.providerTasks, v + offset)
		offset = offset - 1
	end
end

-----------------
--chest storage--
-----------------

local chest = class('chest')
--filter = function(item) returning boolean
chest.initialize = function(self, peri, filter, inputNode, inputDir, outputNode, outputDir)
	self.peri = peri
	self.filter = filter
	self.inputNode = inputNode
	self.inputDir = inputDir
	self.outputNode = outputNode
	self.outputDir = outputDir
	--{entry}
	--	entry = {item = item, qtyContain = qtyContain, qtyToSink = qtyToSink, qtyReserved = qtyReserved, tmpSlots = tmpSlots}
	--		tmpSlots = {tmpSlot}
	--			tmpSlot = {idx = idx, qty = qty}
	self.items = {}
	--{{entry = entry, dstNodeName = dstNodeName, dir = dir, onDone = onDone, qty = qty}}
	self.providerTasks = {}
end
chest.entryForItem = function(self, item, create)
	for k, v in pairs(self.items) do
		if v.item:equals(item) then
			return v
		end
	end
	if create then
		local newEntry = {item = item, qtyContain = 0, qtyToSink = 0, qtyReserved = 0}
		table.insert(self.items, newEntry)
		return newEntry
	else
		return nil
	end
end
local chestProvider = class("chestProvider", provider)
chestProvider.initialize = function(self, chest, priority)
	self.chest = chest
	self.numPriority = priority
end
chestProvider.canProvide = function(self, item, qty)
	local entry = self.chest:entryForItem(item, false)
	if entry ~= nil then
		return entry.qtyContain + entry.qtyToSink - entry.qtyReserved
	else
		return 0
	end
end
chestProvider.reserve = function(self, item, qty)
	local entry = self.chest:entryForItem(item, false)
	if entry == nil then
		error("item to reserve not found in chest: " .. self.chest.peri)
	else
		entry.qtyReserved = entry.qtyReserved + qty
		return {
			cancel = bind(function(entry)
				entry.qtyReserved = entry.qtyReserved - qty
			end, entry),
			perform = bind(function(entry, dstNodeName, dir, onDone)
				table.insert(self.chest.providerTasks, {entry = entry, dstNodeName = dstNodeName, dir = dir, onDone = onDone, qty = qty})
			end, entry)
		}
	end
end
chestProvider.priority = function(self, item, qty)
	return self.numPriority
end
local chestSink = class("chestSink", sink)
chestSink.initialize = function(self, chest, priority)
	self.chest = chest
	self.numPriority = priority
end
chestSink.canSink = function(self, item, qty)
	if self.chest.filter(item) then
		return qty
	else
		return 0
	end
end
chestSink.sink = function(self, item, qty)
	local entry = self.chest:entryForItem(item, true)
	entry.qtyToSink = entry.qtyToSink + qty
	return {
		dstNodeName = self.chest.inputNode,
		dir = self.chest.inputDir,
		qty = qty,
		onDone = bind(function(entry, qtyDone)
			entry.qtyToSink = entry.qtyToSink - qtyDone 
			entry.qtyContain = entry.qtyContain + qtyDone
		end, entry)
	}
end
chestSink.priority = function(self, item, qty)
	return self.numPriority
end
chest.tick = function(self, fm)
	--update contain info
	local stks = pc(self.peri, "getAllStacks")
	if stks == nil then
		print("getAllStacks failed for " .. self.peri)
		return
	end
	for k, v in pairs(self.items) do
		v.qtyContain = 0
		v.tmpSlots = {}
	end
	for k, v in pairs(stks) do
		local stk = v.basic()
		if stk.qty > 0 then
			local item = mcItem:new(stk.id, stk.dmg)
			if not self.filter(item) then
				print("invalid item " .. tostring(item) .. " in chest " .. self.peri .. ", trying to push")
				local rst = pushItemHelper(self.peri, k, self.outputDir, stk.qty)
				if rst == 0 then
					print("failed pushing invalid item")
				else
					if rst < stk.qty then
						print("invalid item stuck")
					end
					fm:onUnexpected(self.outputNode, item, rst)
				end
			else
				local entry = self:entryForItem(item, true)
				entry.qtyContain = entry.qtyContain + stk.qty
				if entry.tmpSlots == nil then entry.tmpSlots = {} end
				table.insert(entry.tmpSlots, {idx = k, qty = stk.qty})
			end
		end
	end
	
	--perform provider tasks
	local idxToRemove = {}
	for k, v in pairs(self.providerTasks) do
		for k1, v1 in pairs(v.entry.tmpSlots) do
			if v.entry.qtyContain == 0 then break end
			if v.qty == 0 then break end
			if v1.qty > 0 then
				local canTake = math.min(v1.qty, v.qty)
				local rst = pushItemHelper(self.peri, v1.idx, self.outputDir, canTake)
				if rst == 0 then
					print("pushItem failed for chest: " .. self.peri .. ", slot " .. v1.idx)
				else
					if rst < canTake then
						print("item stuck at chest near output node: " .. self.outputNode)
					end
					v.entry.qtyContain = v.entry.qtyContain - rst
					v.entry.qtyReserved = v.entry.qtyReserved - rst
					v1.qty = v1.qty - rst
					v.qty = v.qty - rst
					fm:route(v.entry.item, rst, self.outputNode, v.dstNodeName, v.dir, v.onDone)
				end
			end
		end
		if v.qty == 0 then
			table.insert(idxToRemove, k)
		end
	end
	
	local offset = 0
	for k, v in pairs(idxToRemove) do
		table.remove(self.providerTasks, v + offset)
		offset = offset - 1
	end
	
	--prune empty entries
	idxToRemove = {}
	for k, v in pairs(self.items) do
		if v.qtyContain == 0 and v.qtyToSink == 0 and v.qtyReserved == 0 then
			table.insert(idxToRemove, k)
		end
	end
	
	offset = 0
	for k, v in pairs(idxToRemove) do
		table.remove(self.items, v + offset)
		offset = offset - 1
	end
end

-----------------
--crafterSimple--
-----------------

local crafterSimple = class("crafterSimple", crafter)
crafterSimple.initialize = function(self, inpNode, inpDir, outNode, rstItem, rstQty, inputItems, fm)
	self.inpNode = inpNode
	self.inpDir = inpDir
	self.rstItem = rstItem
	self.rstQty = rstQty
	self.outNode = outNode
	self.inputItems = inputItems
	self.nJobs = 0
	self.fm = fm
end
crafterSimple.getResult = function(self)
	return self.rstItem
end
crafterSimple.numResult = function(self)
	return self.rstQty
end
crafterSimple.getMaterials = function(self)
	local rst = {}
	for k, v in pairs(self.inputItems) do
		rst[k] = {item = v, dstNodeName = self.inpNode, dir = self.inpDir}
	end
	return rst
end
crafterSimple.craft = function(self)
	self.nJobs = self.nJobs + 1
	return {
		onMaterialDelivered = function(materialIndex) end,
		onAllMaterialsDelivered = function(destinations)
			self.nJobs = self.nJobs - 1
			for k, v in pairs(destinations) do
				self.fm:route(self.rstItem, v.qty, self.outNode, v.dstNodeName, v.dir, v.onDone)
			end
		end
	}
end
crafterSimple.priority = function(self)
	return 1 / (self.nJobs + 1)
end

-----------------
--crafterControlledOutput--
-----------------

local crafterControlledOutput = class("crafterControlledOutput", crafter)
crafterControlledOutput.initialize = function(self, crafterPeri, outSlot, outDir, inpNode, inpDir, outNode, rstItem, rstQty, inputItems, fm)
	self.crafterPeri = crafterPeri
	self.outSlot = outSlot
	self.outDir = outDir
	self.inpNode = inpNode
	self.inpDir = inpDir
	self.rstItem = rstItem
	self.rstQty = rstQty
	self.outNode = outNode
	self.inputItems = inputItems
	self.nJobs = 0
	self.nOutputsPending = 0
	self.fm = fm
end
crafterControlledOutput.getResult = function(self)
	return self.rstItem
end
crafterControlledOutput.numResult = function(self)
	return self.rstQty
end
crafterControlledOutput.getMaterials = function(self)
	local rst = {}
	for k, v in pairs(self.inputItems) do
		rst[k] = {item = v, dstNodeName = self.inpNode, dir = self.inpDir}
	end
	return rst
end
crafterControlledOutput.craft = function(self)
	self.nJobs = self.nJobs + 1
	return {
		onMaterialDelivered = function(materialIndex) end,
		onAllMaterialsDelivered = function(destinations)
			self.nJobs = self.nJobs - 1
			self.nOutputsPending = self.nOutputsPending + 1
			for k, v in pairs(destinations) do
				self.fm:route(self.rstItem, v.qty, self.outNode, v.dstNodeName, v.dir, v.onDone)
			end
		end
	}
end
crafterControlledOutput.priority = function(self)
	return 1 / (self.nJobs + self.nOutputsPending + 1)
end
crafterControlledOutput.tick = function(self, fm)
	if self.nOutputsPending > 0 then
		local rst = pushItemHelper(self.crafterPeri, self.outSlot, self.outDir, self.nOutputsPending)
		if rst > self.nOutputsPending then
			error("crafterControlledOutput pushed more items than required")
		end
		self.nOutputsPending = self.nOutputsPending - rst
	end
end

---------
--setup--
---------

local function filterOre(item)
	return item.id == "ThermalFoundation:Ore"
		or item.id == "Railcraft:ore"
		or item.id == "minecraft:iron_ore"
		or item.id == "minecraft:redstone_ore"
		or item.id == "minecraft:diamond_ore"
		or item.id == "minecraft:coal_ore"
		or item.id == "minecraft:gold_ore"
		or item.id == "Thaumcraft:blockCustomOre"
		or item.id == "TConstruct:SearedBrick" and item.dmg == 5
		or item.id == "appliedenergistics2:tile.OreQuartz"
		or item.id == "denseores:block0"
		or item.id == "IC2:blockOreUran"
		or item.id == "BigReactors:YelloriteOre"
end

local function filterDef(item)
	return not (
		filterOre(item)
		or item.id == "minecraft:sapling" and item.dmg == 0
		or item.id == "minecraft:log" and item.dmg == 0
		or item.id == "minecraft:apple" and item.dmg == 0
		or item.id == "minecraft:stone" and item.dmg == 0
		or item.id == "minecraft:dirt" and item.dmg == 0
		or item.id == "minecraft:cobblestone" and item.dmg == 0
		or item.id == "minecraft:dirt" and item.dmg == 1
		or item.id == "minecraft:sand" and item.dmg == 0
		or item.id == "minecraft:gravel" and item.dmg == 0
		or item.id == "Forestry:decayingWheat" and item.dmg == 0
	)
end

local fm = factoryManager:new()
local toTick = {}
local function addSimpleNode(name, peri)
	local node = nodeSimple:new(peri)
	table.insert(toTick, node)
	fm:addNode(name, node)
end
addSimpleNode("aboveOakSapling", "dropper_0")
addSimpleNode("aboveOakLog", "dropper_1")
addSimpleNode("aboveApple", "dropper_2")
addSimpleNode("nearDef1", "dropper_3")
addSimpleNode("nearOre1", "dropper_4")
addSimpleNode("aboveDirt", "dropper_5")
addSimpleNode("aboveStone", "dropper_6")
addSimpleNode("abovePul1", "dropper_7")
addSimpleNode("aboveFurn1", "dropper_8")
addSimpleNode("aboveCobble", "dropper_10")
addSimpleNode("nearDef2", "dropper_11")
addSimpleNode("aboveCDirt", "dropper_12")
addSimpleNode("aboveSand", "dropper_13")
addSimpleNode("aboveGravel", "dropper_14")

fm:addRoute("aboveOakSapling", "east", "aboveOakLog")
fm:addRoute("aboveOakLog", "west", "aboveOakSapling")
fm:addRoute("aboveOakLog", "east", "aboveApple")
fm:addRoute("aboveApple", "west", "aboveOakLog")
fm:addRoute("aboveApple", "up", "nearDef1")
fm:addRoute("nearDef1", "up", "aboveApple")
fm:addRoute("nearDef1", "east", "nearOre1")
fm:addRoute("nearOre1", "west", "nearDef1")
fm:addRoute("nearOre1", "up", "aboveDirt")
fm:addRoute("aboveDirt", "west", "aboveStone")
fm:addRoute("aboveStone", "east", "aboveDirt")
fm:addRoute("aboveStone", "west", "nearDef1")
fm:addRoute("aboveOakSapling", "west", "abovePul1")
fm:addRoute("abovePul1", "east", "aboveOakSapling")
fm:addRoute("abovePul1", "west", "aboveFurn1")
fm:addRoute("aboveFurn1", "east", "abovePul1")
fm:addRoute("aboveDirt", "east", "aboveCobble")
fm:addRoute("aboveCobble", "west", "aboveDirt")
fm:addRoute("nearOre1", "east", "nearDef2")
fm:addRoute("nearDef2", "west", "nearOre1")
fm:addRoute("aboveCDirt", "west", "aboveCobble")
fm:addRoute("aboveCobble", "east", "aboveCDirt")
fm:addRoute("aboveCDirt", "east", "aboveSand")
fm:addRoute("aboveSand", "east", "aboveGravel")
fm:addRoute("aboveGravel", "west", "aboveSand")
fm:addRoute("aboveSand", "west", "aboveCDirt")


local barrelOakSapling = barrel:new("mcp_mobius_betterbarrel_0", mcItem:new("minecraft:sapling", 0), 32768, "aboveOakSapling", "down", "aboveOakSapling", "up")
fm:addProvider(barrelProvider:new(barrelOakSapling, 1))
fm:addSink(barrelSink:new(barrelOakSapling, 1))
table.insert(toTick, barrelOakSapling)

local barrelOakLog = barrel:new("mcp_mobius_betterbarrel_1", mcItem:new("minecraft:log", 0), 57344, "aboveOakLog", "down", "aboveOakLog", "up")
fm:addProvider(barrelProvider:new(barrelOakLog, 1))
fm:addSink(barrelSink:new(barrelOakLog, 1))
table.insert(toTick, barrelOakLog)

local barrelApple = barrel:new("mcp_mobius_betterbarrel_2", mcItem:new("minecraft:apple", 0), 8192, "aboveApple", "down", "aboveApple", "up")
fm:addProvider(barrelProvider:new(barrelApple, 1))
fm:addSink(barrelSink:new(barrelApple, 1))
table.insert(toTick, barrelApple)

local barrelDirt = barrel:new("mcp_mobius_betterbarrel_3", mcItem:new("minecraft:dirt", 0), 12288, "aboveDirt", "down", "aboveDirt", "up")
fm:addProvider(barrelProvider:new(barrelDirt, 1))
fm:addSink(barrelSink:new(barrelDirt, 1))
table.insert(toTick, barrelDirt)

local barrelStone = barrel:new("mcp_mobius_betterbarrel_4", mcItem:new("minecraft:stone", 0), 4096, "aboveStone", "down", "aboveStone", "up")
fm:addProvider(barrelProvider:new(barrelStone, 1))
fm:addSink(barrelSink:new(barrelStone, 1))
table.insert(toTick, barrelStone)

local barrelCobble = barrel:new("mcp_mobius_betterbarrel_5", mcItem:new("minecraft:cobblestone", 0), 53248, "aboveCobble", "down", "aboveCobble", "up")
fm:addProvider(barrelProvider:new(barrelCobble, 1))
fm:addSink(barrelSink:new(barrelCobble, 1))
table.insert(toTick, barrelCobble)

local barrelCDirt = barrel:new("mcp_mobius_betterbarrel_6", mcItem:new("minecraft:dirt", 1), 4096, "aboveCDirt", "down", "aboveCDirt", "up")
fm:addProvider(barrelProvider:new(barrelCDirt, 1))
fm:addSink(barrelSink:new(barrelCDirt, 1))
table.insert(toTick, barrelCDirt)

local barrelSand = barrel:new("mcp_mobius_betterbarrel_7", mcItem:new("minecraft:sand", 0), 4096, "aboveSand", "down", "aboveSand", "up")
fm:addProvider(barrelProvider:new(barrelSand, 1))
fm:addSink(barrelSink:new(barrelSand, 1))
table.insert(toTick, barrelSand)

local barrelGravel = barrel:new("mcp_mobius_betterbarrel_8", mcItem:new("minecraft:gravel", 0), 4096, "aboveGravel", "down", "aboveGravel", "up")
fm:addProvider(barrelProvider:new(barrelGravel, 1))
fm:addSink(barrelSink:new(barrelGravel, 1))
table.insert(toTick, barrelGravel)

local barrelDWheat = barrel:new("mcp_mobius_betterbarrel_9", mcItem:new("Forestry:decayingWheat", 0), 4096, "aboveCobble", "up", "aboveCobble", "down")
fm:addProvider(barrelProvider:new(barrelDWheat, 1))
fm:addSink(barrelSink:new(barrelDWheat, 1))
table.insert(toTick, barrelDWheat)

local chestDef1 = chest:new("iron_0", filterDef, "nearDef1", "south", "nearDef1", "north")
fm:addProvider(chestProvider:new(chestDef1, 2))
fm:addSink(chestSink:new(chestDef1, 0.5))
table.insert(toTick, chestDef1)

local chestOre1 = chest:new("iron_1", filterOre, "nearOre1", "south", "nearOre1", "north")
fm:addProvider(chestProvider:new(chestOre1, 1))
fm:addSink(chestSink:new(chestOre1, 1))
table.insert(toTick, chestOre1)

local chestDef2 = chest:new("iron_2", filterDef, "nearDef2", "south", "nearDef2", "north")
fm:addProvider(chestProvider:new(chestDef2, 1))
fm:addSink(chestSink:new(chestDef2, 1))
table.insert(toTick, chestDef2)

local planterPeri = "planter_0"
local planterReserved = 0
local planterItem = mcItem:new("minecraft:sapling", 0)
local planterInputNode = "aboveOakSapling"
local planterInputDir = "up"
local function doPlanterTick()
	local accum = 0
	local stks = pc(planterPeri, "getAllStacks")
	if stks == nil then
		print("getAllStacks for " .. planterPeri .. " failed")
		return
	end
	for k, v in pairs(stks) do
		local stk = v.basic()
		local item = mcItem:new(stk.id, stk.dmg)
		if item:equals(planterItem) then
			accum = accum + stk.qty
		end
	end
	local need = 32 - (planterReserved + accum)
	if need > 0 then
		local plan = fm:planCrafting(planterItem, need, planterInputNode, planterInputDir)
		if plan.qtyAvail < need then
			print("insufficient sapling")
		end
		planterReserved = planterReserved + plan.qtyAvail
		plan.perform(function(qtyDone)
			planterReserved = planterReserved - qtyDone
		end)
	end
end
table.insert(toTick, {tick = doPlanterTick})

fm:addCrafter(crafterSimple:new("abovePul1", "down", "abovePul1", mcItem:new("minecraft:sand", 0), 1, {mcItem:new("minecraft:cobblestone", 0)}, fm))
fm:addCrafter(crafterSimple:new("aboveFurn1", "down", "abovePul1", mcItem:new("minecraft:glass", 0), 1, {mcItem:new("minecraft:sand", 0)}, fm))
fm:addCrafter(crafterSimple:new("aboveFurn1", "down", "abovePul1", mcItem:new("ExtraUtilities:decorativeBlock2", 0), 1, {mcItem:new("ExtraUtilities:decorativeBlock1", 9)}, fm))

local cobbleProvider = class("cobbleProvider", provider)
cobbleProvider.initialize = function(self, peri, outNode, outDir)
	self.peri = peri
	self.outNode = outNode
	self.outDir = outDir
	--{{dstNodeName = dstNodeName, dir = dir, qty = qty, onDone = onDone}}
	self.jobs = {}
end
cobbleProvider.canProvide = function(self, item, qty)
	if item:equals(mcItem:new("minecraft:cobblestone", 0)) then
		return qty
	end
	return 0
end
cobbleProvider.reserve = function(self, item, qty)
	return {
		cancel = function() end,
		perform = function(dstNodeName, dir, onDone)
			table.insert(self.jobs, {dstNodeName = dstNodeName, dir = dir, onDone = onDone, qty = qty})
		end
	}
end
cobbleProvider.priority = function(self, item, qty) return 0.5 end
cobbleProvider.tick = function(self, fm)
	local idxToRemove = {}
	for k, v in pairs(self.jobs) do
		local rst = pushItemHelper(self.peri, 1, self.outDir, v.qty)
		if rst == 0 then print("not enough cobbles") break end
		if rst > v.qty then error("cobbleProvider pushed more item than wanted") end
		v.qty = v.qty - rst
		if v.qty == 0 then table.insert(idxToRemove, k) end
		fm:route(mcItem:new("minecraft:cobblestone", 0), rst, self.outNode, v.dstNodeName, v.dir, v.onDone)
	end
	local offset = 0
	for k, v in pairs(idxToRemove) do
		table.remove(self.jobs, v + offset)
		offset = offset - 1
	end
end
local cobbleProviderInst = cobbleProvider:new("tile_thermalexpansion_machine_extruder_name_0", "aboveStone", "down")
fm:addProvider(cobbleProviderInst)
table.insert(toTick, cobbleProviderInst)

-----------
--request--
-----------

--{requestJob}
--requestJob = {item = item, qtyOrdered = qtyOrdered, qtyRequested = qtyRequested, qtyDelivered = qtyDelivered}
local requestJobs = {}
local requestInputNode = "abovePul1"
local requestInputDir = "up"

function processRequestJobs()
	local idxToRemove = {}
	for k, v in pairs(requestJobs) do
		if v.qtyOrdered == 0 and v.qtyRequested == v.qtyDelivered then
			table.insert(idxToRemove, k)
		else
			if v.qtyOrdered + v.qtyDelivered < v.qtyRequested then
				local needed = v.qtyRequested - (v.qtyOrdered + v.qtyDelivered)
				local rst = fm:planCrafting(v.item, math.min(needed, 64), requestInputNode, requestInputDir)
				v.qtyOrdered = v.qtyOrdered + rst.qtyAvail
				rst.perform(bind(function(v, qtyDone)
					v.qtyDelivered = v.qtyDelivered + qtyDone
					v.qtyOrdered = v.qtyOrdered - qtyDone
				end, v))
			end
			print("#" .. k .. "(" .. tostring(v.item) .. ") " .. v.qtyDelivered .. "+" .. v.qtyOrdered .. "/" .. v.qtyRequested)
		end
	end
	for k, v in pairs(idxToRemove) do
		requestJobs[v] = nil
	end
end

function delayAndAskForRequest()
	local timer = os.startTimer(3)
	local isChar
	while true do
		local ev, a1, a2 = os.pullEvent()
		if ev == "timer" and a1 == timer then
			isChar = false
			break
		elseif ev == "char" and string.lower(a1) == 't' then
			isChar = true
			break
		end
	end
	if isChar then
		print("id (or cancel)?")
		local id = io.read('*l')
		if id == 'cancel' then
			print("job number?")
			id = tonumber(io.read('*l'))
			requestJobs[id] = nil
		else
			print("dmg?")
			local dmg = tonumber(io.read('*l'))
			print("qty?")
			local qty = tonumber(io.read('*l'))
			requestJobs[#requestJobs + 1] = {item = mcItem:new(id, dmg), qtyRequested = qty, qtyOrdered = 0, qtyDelivered = 0}
		end
	end
end

-------------
--main loop--
-------------

while true do
	local t1 = os.time()
	processRequestJobs()
	for k, v in pairs(toTick) do
		v:tick(fm)
		io.write(">")
	end
	local t2 = os.time()
	print(math.floor(1000 * (t2 - t1)) .. " ticks")
	delayAndAskForRequest()
end


