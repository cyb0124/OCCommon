-- by cybcaoyibo

local meMain = "back"

local monitors = {{name = "left", scale = 0.5}}

local unpack = function(tab)
	if table.unpack ~= nil then return table.unpack(tab) end
	return _G.unpack(tab)
end

local pack = function(...)
	return {...}
end

local pc = function(...)
	local rst = pack(pcall(peripheral.call, unpack({...})))
	if rst[1] == false then return nil end
	return select(2, unpack(rst))
end

local terms = {term}
for i = 1, #monitors do
  local name = monitors[i].name
  local scale = monitors[i].scale
  table.insert(terms, setmetatable({}, {__index = function(tab, key)
    return function(...)
      if key == "clear" then pc(name, "setTextScale", scale) end
      return pc(name, key, ...)
    end
  end}))
end
local termY = 1

local print = function(x, color)
  for i = 1, #terms do
    if color ~= nil then terms[i].setTextColor(color)
    else terms[i].setTextColor(colors.white) end
    terms[i].setCursorPos(1, termY)
    terms[i].write(x)
  end
  termY = termY + 1
end

local cycleId = 0

while true do
  termY = 1
  for i = 1, #terms do
    terms[i].setBackgroundColor(colors.black)
    terms[i].clear()
  end
  print("Factory Controller by cybcaoyibo - Cycle #" .. cycleId, colors.lime)
  
  -- Item Gathering
  
  local rawItems = pc(meMain, "getAvailableItems")
  if rawItems == nil then
    print("getAvailableItems failed on meMain", colors.red)
    rawItems = {}
  end
  
  local items = {}
  local nameMap = {}
  local idMap = {}
  
  for _, rawItem in pairs(rawItems) do
    local item = pc(meMain, "getItemDetail", rawItem.fingerprint)
    if item == nil then
      print("getItemDetail failed on meMain", colors.red)
    else
      ok, item = pcall(item.all)
      if not ok then
        print("all() failed on item detail", colors.red)
      elseif item.display_name == nil then
        print("item doesn't have display_name", colors.red)
      elseif item.id == nil then
        print("item doesn't have id", colors.red)
      else
        item.identifier = rawItem.fingerprint
        nameMap[item.display_name] = item
        idMap[item.id] = item
        table.insert(items, item)
      end
    end
  end
  
  local pushItem = function(me, item, direction, qty, intoSlot)
    if item == nil then return 0 end
    local result = pc(me, "exportItem", item.identifier, direction, qty, intoSlot)
    if result == nil then
      print("exportItem failed on " .. me, colors.red)
      return 0
    else
      result = result.size
      print(item.display_name .. "*" .. result .. "/" .. qty)
      item.qty = item.qty - result
      return result
    end
  end
  
  -- Test
  
  pushItem("back", nameMap["Iron Ingot"], "west", 4)
  
  -- End of Cycle
  
  local timer = os.startTimer(1)
  while true do
    local ev, eArg = os.pullEvent()
    if ev == "timer" and eArg == timer then
      break
    elseif ev == "key" then
      if eArg == keys.x then
        os.cancelTimer(timer)
        return
      end
    end
  end
  cycleId = cycleId + 1
end