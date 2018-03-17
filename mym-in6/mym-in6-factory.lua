-- by cybcaoyibo

local infMain = "tileinterface_5"
local monitors = {{name = "left", scale = 0.7}}

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

local pcLogError = function(...)
	local rst = pack(pcall(peripheral.call, unpack({...})))
	if rst[1] == false then
    print(select(2, ...) .. " failed on " .. select(1, ...), colors.red)
    return nil
  end
	return select(2, unpack(rst))
end

local pcLogErrorAndNil = function(...)
	local rst = pack(pcall(peripheral.call, unpack({...})))
	if rst[1] == false or rst[2] == nil then
    print(select(2, ...) .. " failed on " .. select(1, ...), colors.red)
    return nil
  end
	return select(2, unpack(rst))
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

  local rawItems = pcLogErrorAndNil(infMain, "getAvailableItems", "all")
  if rawItems ~= nil then
    local items = {}
    local nameMap = {}
    local idMap = {}

    for _, rawItem in pairs(rawItems) do
      local item = rawItem.item
      if item == nil then
        print("item doesn't have detail", colors.red)
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

    local pushItem = function(me, item, direction, qty, intoSlot)
      if item == nil or item.qty <= 0 or qty <= 0 then return 0 end
      local result = pcLogErrorAndNil(
        me, "exportItem", item.identifier, direction, qty, intoSlot)
      if result == nil then
        return 0
      else
        result = result.size
        print(item.display_name .. "*" .. result .. "/" .. qty)
        item.qty = item.qty - result
        return result
      end
    end

    local getQty = function(item)
      if item == nil or item.qty <= 0 then return 0 end
      return item.qty
    end

    local getItem = function(item)
      if type(item) == "string" then return nameMap[item] end
      return item
    end

    local rankRecipes = function(recipes, adjuest)
      for _, v in pairs(recipes) do
        local i = getQty(getItem(v.i))
        local o = getQty(getItem(v.o))
        if i <= 0 then v.demand = 0
        else v.demand = math.max(0, (v.qty - o) / v.qty) end
        if adjuest ~= nil then adjuest(v) end
      end
      table.sort(recipes, function(x, y) return x.demand > y.demand end)
    end

    local forEachSlot = function(inv, f)
      local stacks = pcLogErrorAndNil(inv, "getAllStacks")
      if stacks == nil then return false end
      for _, item in pairs(stacks) do
        f(item.all())
      end
      return true
    end

    -- Ambiguous Names
    local rubberWood = idMap["MineFactoryReloaded:rubberwood.log"]

    -- CobbleGen

    do
      local inv = "tile_thermalexpansion_machine_extruder_name_5"
      local dir = "east"
      local avail = getQty(getItem("Cobblestone"))
      local toStock = 100000
      if avail < toStock then
        local stack = pcLogError(inv, "getStackInSlot", 1)
        if stack ~= nil and stack.qty > 1 then
          local result = pcLogErrorAndNil(
            inv, "pushItem", dir, 1, math.min(stack.qty - 1, toStock - avail))
          if result ~= nil then
            print("CobbleGen: " .. result .. "/" .. (toStock - avail))
          end
        end
      end
    end

    -- ObsidianGen

    do
      local inv = "tile_thermalexpansion_machine_extruder_name_4"
      local dir = "east"
      local avail = getQty(getItem("Obsidian"))
      local toStock = 4096
      if avail < toStock then
        local stack = pcLogError(inv, "getStackInSlot", 1)
        if stack ~= nil and stack.qty > 1 then
          local result = pcLogErrorAndNil(
            inv, "pushItem", dir, 1, math.min(stack.qty - 1, toStock - avail))
          if result ~= nil then
            print("ObsidianGen: " .. result .. "/" .. (toStock - avail))
          end
        end
      end
    end

    -- Tree farm

    do
      local harvester = "harvester_3"
      local inv = "planter_2"
      local inf = "tileinterface_3"
      local dir = "up"
      local recipes = {
        {i = "Oak Sapling",    o = "Oak Sapling",    qty = 256,  shear = false},
        {i = "Rubber Sapling", o = "Rubber Sapling", qty = 256,  shear = false},
        {i = "Oak Sapling",    o = "Oak Wood",       qty = 4096, shear = false},
        {i = "Oak Sapling",    o = "Oak Leaves",     qty = 4096, shear = true},
        {i = "Oak Sapling",    o = "Apple",          qty = 4096, shear = false},
        {i = "Rubber Sapling", o = rubberWood,       qty = 4096, shear = false},
        {i = "Rubber Sapling", o = "Rubber Leaves",  qty = 4096, shear = true},
        {i = "Rubber Sapling", o = "Raw Rubber",     qty = 4096, shear = false},
        {i = "Seeds",          o = "Wheat",          qty = 4096, shear = false},
        {i = "Seeds",          o = "Seeds",          qty = 4096, shear = false},
        {i = "Carrot",         o = "Carrot",         qty = 4096, shear = false},
        {i = "Potato",         o = "Potato",         qty = 4096, shear = false},
        {i = "Industrial Hemp Seeds", o = "Industrial Hemp Seeds", qty = 4096, shear = false},
        {i = "Industrial Hemp Seeds", o = "Industrial Hemp Fiber", qty = 4096, shear = false}
      }
      rankRecipes(recipes, function(x)
        if type(x.o) == "string" and
            (string.match(x.o, "Sapling$") ~= nil
            or string.match(x.o, "Seeds$") ~= nil) then
          if x.demand > 0 then x.demand = 2 + x.demand end
        end
      end)
      if recipes[1].demand > 0 then
        pcLogError(harvester, "setShearLeaves", recipes[1].shear)
        local inproc = 0
        if forEachSlot(inv, function(x) inproc = inproc + x.qty end) then
          pushItem(inf, getItem(recipes[1].i), dir, 8 - inproc)
        end
      end
    end

    -- Pulverizer

    do
      local inf = "tileinterface_14"
      local dir = "down"
      local inv = "tile_thermalexpansion_machine_pulverizer_name_1"
      local ores = {
        "Copper Ore",
        "Iron Ore",
        "Gold Ore"
      }
      local inproc = 0
      local stack = pcLogError(inv, "getStackInSlot", 1)
      if stack ~= nil then inproc = stack.qty end
      for _, v in pairs(ores) do
        local item = getItem(v)
        local qty = getQty(item)
        inproc = inproc + pushItem(inf, item, dir, math.min(qty, 8 - inproc))
      end
      local recipes = {
        {i = "Cobblestone", o = "Sand",                qty = 4096},
        {i = "Sand",        o = "Silicon",             qty = 4096},
        {i = "Obsidian",    o = "Pulverized Obsidian", qty = 4096},
        {i = "Oak Wood",    o = "Sawdust",             qty = 4096},
        {i = "Certus Quartz Crystal", o = "Certus Quartz Dust", qty = 16},
        {i = "Nether Quartz",         o = "Nether Quartz Dust", qty = 16},
        {i = "Fluix Crystal",         o = "Fluix Dust",         qty = 16}
      }
      rankRecipes(recipes)
      for i = 1, #recipes do
        local recipe = recipes[i]
        if recipe.demand > 0 then
          inproc = inproc + pushItem(inf, getItem(recipes[i].i), dir,
            math.min(8 - inproc,
              recipes[i].qty - getQty(getItem(recipes[i].o))) - inproc)
        else
          break
        end
      end
    end

    -- Furnace

    do
      local inf = "tileinterface_9"
      local dir = "east"
      local inv = "tile_thermalexpansion_machine_furnace_name_1"
      local inproc = 0
      local stack = pcLogError(inv, "getStackInSlot", 1)
      if stack ~= nil then inproc = stack.qty end
      local recipes = {
        {i = "Sand",     o = "Glass",    qty = 4096},
        {i = "Oak Wood", o = "Charcoal", qty = 4096}
      }
      rankRecipes(recipes)
      for i = 1, #recipes do
        local recipe = recipes[i]
        if recipe.demand > 0 then
          inproc = inproc + pushItem(inf, getItem(recipes[i].i), dir,
            math.min(8 - inproc,
              recipes[i].qty - getQty(getItem(recipes[i].o))) - inproc)
        else
          break
        end
      end
    end

    -- Charger

    do
      local inf = "tileinterface_5"
      local dir = "east"
      local inv = "tile_extrautils_chestfull_name_1"
      local toStock = 16
      local maxInproc = 32
      local avail = getQty(getItem("Charged Certus Quartz Crystal"))
      if avail < toStock then
        local inproc = 0
        if forEachSlot(inv, function(x) inproc = inproc + x.qty end) then
          pushItem(inf, getItem("Certus Quartz Crystal"), dir,
            math.min(maxInproc - inproc, toStock - avail - inproc))
        end
      end
    end

    -- Fluix

    do
      local inf = "tileinterface_6"
      local dir = "east"
      local toStock = 16
      local avail = getQty(getItem("Fluix Crystal"))
      if avail < toStock then
        local i1 = getItem("Charged Certus Quartz Crystal")
        local i2 = getItem("Redstone")
        local i3 = getItem("Nether Quartz")
        local amt = math.min(8, getQty(i1), getQty(i2), getQty(i3))
        pushItem(inf, i1, dir, amt)
        pushItem(inf, i2, dir, amt)
        pushItem(inf, i3, dir, amt)
      end
    end

    -- Crystal Seeds

    do
      local inv = "crafter_inventory_0"
      local inf = "tileinterface_6"
      local dir = "south"
      local sandToStock = 48

      local sand = 0
      local inproc = 0
      if forEachSlot(inv, function(item)
            if item.display_name == "Sand" then sand = sand + item.qty
            else inproc = inproc + item.qty end
          end) then
        pushItem(inf, getItem("Sand"), dir, sandToStock - sand)
        local recipes = {
          {i = "Certus Quartz Dust", o = "Certus Quartz Seed", qty = 16},
          {i = "Nether Quartz Dust", o = "Nether Quartz Seed", qty = 16},
          {i = "Fluix Dust",         o = "Fluix Seed",         qty = 16}
        }
        rankRecipes(recipes)
        for i = 1, #recipes do
          local recipe = recipes[i]
          if recipe.demand > 0 then
            inproc = inproc + pushItem(inf, getItem(recipes[i].i), dir,
              math.min(8 - inproc,
                recipes[i].qty - getQty(getItem(recipes[i].o))) - inproc)
          end
        end
      end
    end

    -- Pure Crystals

    do
      local inf = "tileinterface_8"
      local dir = "east"
      local toStock = 256
      local recipes = {"Certus Quartz", "Nether Quartz", "Fluix"}
      for _, recipe in pairs(recipes) do
        local input = getItem(recipe .. " Seed")
        local output = getItem("Pure " .. recipe .. " Crystal")
        if getQty(output) < toStock then
          pushItem(inf, input, dir, 1)
        end
      end
    end

    -- Inscriber Circuits

    do
      local toStock = 16
      local maxInproc = 32
      local recipes = {
        {
          inv = "tile_extrautils_chestfull_name_0",
          inf = "tileinterface_10",
          dir = "south",
          i = "Diamond",
          o = "Printed Engineering Circuit"
        }, {
          inv = "tile_extrautils_chestfull_name_2",
          inf = "tileinterface_11",
          dir = "south",
          i = "Silicon",
          o = "Printed Silicon"
        }, {
          inv = "tile_extrautils_chestfull_name_3",
          inf = "tileinterface_12",
          dir = "south",
          i = "Pure Certus Quartz Crystal",
          o = "Printed Calculation Circuit"
        }, {
          inv = "tile_extrautils_chestfull_name_4",
          inf = "tileinterface_13",
          dir = "south",
          i = "Gold Ingot",
          o = "Printed Logic Circuit"
        }
      }
      for _, recipe in pairs(recipes) do
        local avail = getQty(getItem(recipe.o))
        if avail < toStock then
          local inproc = 0
          if forEachSlot(recipe.inv, function(x) inproc = inproc + x.qty end) then
            pushItem(recipe.inf, getItem(recipe.i), recipe.dir, math.min(
              maxInproc - inproc, toStock - avail - inproc))
          end
        end
      end
    end

    -- Inscriber Processors

    do
      local inv = "tile_extrautils_chestfull_name_5"
      local inf = "tileinterface_15"
      local dir = "south"
      local toStock = 64
      local maxInproc = 32

      local redstone = 0
      local silicon = 0
      local inproc = 0
      if forEachSlot(inv, function(item)
            if item.display_name == "Redstone" then
              redstone = redstone + item.qty
            elseif item.display_name == "Printed Silicon" then
              silicon = silicon + item.qty
            else
              inproc = inproc + item.qty
            end
          end) then
        pushItem(inf, getItem("Redstone"), dir, math.min(maxInproc - redstone))
        pushItem(inf, getItem("Printed Silicon"), dir, math.min(maxInproc - silicon))
        local recipes = {
          {i = "Printed Logic Circuit",       o = "Logic Processor",       qty = toStock},
          {i = "Printed Engineering Circuit", o = "Engineering Processor", qty = toStock},
          {i = "Printed Calculation Circuit", o = "Calculation Processor", qty = toStock}
        }
        rankRecipes(recipes)
        for i = 1, #recipes do
          local recipe = recipes[i]
          if recipe.demand > 0 then
            inproc = inproc + pushItem(inf, getItem(recipe.i), dir,
              math.min(maxInproc - inproc,
                recipe.qty - getQty(getItem(recipe.o)) - inproc))
          end
        end
      end
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
