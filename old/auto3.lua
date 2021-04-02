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

