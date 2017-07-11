-- by cybcaoyibo

local drawerMain = "back"
local drawerPulverizer = "back"
local dirPulverizer = "up"
local drawerCompressor = "storagedrawers_container_controller_4"
local dirCompressor = "up"
local drawerHammer = "storagedrawers_container_controller_5"
local dirHammer = "up"
local drawerFurnace = "storagedrawers_container_controller_6"
local dirFurnace = "up"
local drawerSieve = "storagedrawers_container_controller_7"
local dirSieve = "up"
local drawerIngots = "storagedrawers_container_controller_4"
local dirIngots = "south"
local drawerCertus = "storagedrawers_container_controller_7"
local dirCertus = "south"
local drawerSoulSand = "back"
local dirSoulSand = "west"
local drawerInvalid = "storagedrawers_container_controller_6"
local dirInvalid = "south"

local monitors = {{name = "monitor_27", scale = 0.5}}
local sieveScheduling = {
  {name = "Gravel", qty = 16},
  {name = "Sand", qty = 16},
  {name = "Dust", qty = 16},
  {name = "Soul Sand", qty = 16}
}

local toStock = 64
local requestAmount = 8

local patternGravel = " Ore Gravel$"
local patternSand = " Ore Sand$"
local patternDust = " Ore Dust$"
local patternBroken = "^Broken .+ Ore$"
local patternCrushed = "^Crushed .+ Ore$"
local patternPowdered = "^Powdered .+ Ore$"
local patternIngot = " Ingot$"
local patternCertus = "Certus Quartz Crystal$"
local isMetalPowder = function(name)
  if string.match(name, " Grit$") ~= nil then return true end
  if name == "Yellorium Dust" then return true end
  local result = string.match(name, "^Pulverized (.*)$")
  if result ~= nil and result ~= "Coal"
        and result ~= "Charcoal" and result ~= "Obsidian"
        then
    return true
  end
  return false
end

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
local currentSievingItem = 1
local currentSievingDone = 0

while true do
  termY = 1
  for i = 1, #terms do
    terms[i].setBackgroundColor(colors.black)
    terms[i].clear()
  end
  print("Factory Controller by cybcaoyibo - Cycle #" .. cycleId, colors.lime)
  print("Sieving " .. sieveScheduling[currentSievingItem].name
      .. "*" .. currentSievingDone .. "/" .. sieveScheduling[currentSievingItem].qty,
      colors.lime)
  
  -- Item Gathering
  
  local stacks = pc(drawerMain, "getAllStacks")
  if stacks == nil then
    print("getAllStacks failed on drawerMain", colors.red)
    stacks = {}
  end
  
  local items = {}
  local getItem = function(name)
    local result = items[name]
    if result == nil then
      result = {
        qty = 0,
        providers = {}
      }
      items[name] = result
    end
    return result
  end
  
  local pushItem = function(drawer, itemName, direction, qty, intoSlot)
    if qty <= 0 then return 0 end
    local result = 0
    local item = getItem(itemName)
    for _, provider in pairs(item.providers) do
      if qty <= 0 then break end
      if provider.qty > 0 then
        local resultNow = pc(drawer, "pushItem", direction, provider.slot, math.min(qty, provider.qty), intoSlot)
        if resultNow == nil then
          print("pushItem failed on " .. drawer, colors.red)
        else
          result = result + resultNow
          item.qty = item.qty - resultNow
          provider.qty = provider.qty - resultNow
          qty = qty - resultNow
        end
      end
    end
    print(itemName .. "*" .. result .. "/" .. (result + qty))
    return result
  end
  
  for slot, v in pairs(stacks) do
    local i = v.basic()
    local name = i.display_name
    if i.id == "exnihilo:stone" then name = "(Stone)"
    elseif name == nil then name = "Unknown" end
    local item = getItem(name)
    item.qty = item.qty + i.qty
    table.insert(item.providers, {
      slot = slot,
      qty = i.qty
    })
  end
  
  for _, item in pairs(items) do
    table.sort(item.providers, function(x, y)
      return x.qty < y.qty
    end)
  end
  
  -- Pulverizer
  
  local pulverizerHasWork = false
  for name, item in pairs(items) do
    if item.qty > 0 and string.match(name, patternDust) ~= nil then
      pushItem(drawerPulverizer, name, dirPulverizer, item.qty)
      pulverizerHasWork = true
    end
  end
  if pulverizerHasWork then
  elseif getItem("Gravel").qty < toStock then
    pushItem(drawerPulverizer, "Cobblestone", dirPulverizer, requestAmount)
  elseif getItem("Sand").qty < toStock then
    pushItem(drawerPulverizer, "Gravel", dirPulverizer, requestAmount)
  elseif getItem("Dust").qty < toStock then
    pushItem(drawerPulverizer, "Sand", dirPulverizer, requestAmount)
  end
  
  -- Soul Sand
  
  if getItem("Soul Sand").qty < toStock then
    pushItem(drawerSoulSand, "Sand", dirSoulSand, requestAmount)
  end
  
  -- Compressor
  
  for name, item in pairs(items) do
    if item.qty >= 4 then
      if name == "(Stone)"
          or string.match(name, patternBroken) ~= nil
          or string.match(name, patternCrushed) ~= nil
          or string.match(name, patternPowdered) ~= nil
          then
        local qty = math.floor(item.qty / 4) * 4
        pushItem(drawerCompressor, name, dirCompressor, qty)
        break
      end
    end
  end
  
  -- Hammer
  
  for name, item in pairs(items) do
    if item.qty > 0 then
      if string.match(name, patternGravel) ~= nil
          or string.match(name, patternSand) ~= nil
          then
        pushItem(drawerHammer, name, dirHammer, item.qty)
        break
      end
    end
  end
  
  -- Furnace
  
  for name, item in pairs(items) do
    if item.qty > 0 then
      if isMetalPowder(name) then
        pushItem(drawerFurnace, name, dirFurnace, item.qty)
        break
      end
    end
  end
  
  -- Sieve
  
  do
    local remaining = sieveScheduling[currentSievingItem].qty - currentSievingDone
    local newlyDone = pushItem(drawerSieve, sieveScheduling[currentSievingItem].name, dirSieve, remaining)
    currentSievingDone = currentSievingDone + newlyDone
    remaining = remaining - newlyDone
    if remaining <= 0 then
      if currentSievingItem == #sieveScheduling then
        currentSievingItem = 1
      else
        currentSievingItem = currentSievingItem + 1
      end
      currentSievingDone = 0
    end
  end
  
  -- Dispose Invalid Items
  
  getItem("Cobblestone").valid = true
  getItem("Gravel").valid = true
  getItem("Sand").valid = true
  getItem("Dust").valid = true
  getItem("Dirt").valid = true
  getItem("Soul Sand").valid = true
  getItem("(Stone)").valid = true
  for name, item in pairs(items) do
    if item.valid then
    elseif string.match(name, patternGravel) ~= nil then
    elseif string.match(name, patternSand) ~= nil then
    elseif string.match(name, patternDust) ~= nil then
    elseif string.match(name, patternBroken) ~= nil then
    elseif string.match(name, patternCrushed) ~= nil then
    elseif string.match(name, patternPowdered) ~= nil then
    elseif isMetalPowder(name) then
    elseif string.match(name, patternIngot) ~= nil then
      pushItem(drawerIngots, name, dirIngots, item.qty)
    elseif string.match(name, patternCertus) ~= nil then
      pushItem(drawerCertus, name, dirCertus, item.qty)
    else
      pushItem(drawerInvalid, name, dirInvalid, item.qty)
    end
  end
  
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
