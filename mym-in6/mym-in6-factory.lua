-- by cybcaoyibo

local waitCycle = 1
local waitThrottlePC = 0
local waitThrottleAE = 0.75
local modem = "back"
local infMain = "tileinterface_5"
local monitors = {{name = "monitor_244", scale = 0.7}}

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
local termX = 1

local print = function(x, color)
  for i = #x + 1, termX do x = x .. " " end
  for i = 1, #terms do
    if color ~= nil then terms[i].setTextColor(color)
    else terms[i].setTextColor(colors.white) end
    terms[i].setCursorPos(1, termY)
    terms[i].write(pad)
    terms[i].setCursorPos(1, termY)
    terms[i].write(x)
  end
  termY = termY + 1
  termX = 1
end

local wait = function(x)
  if x > 0 then
    for i = 1, #terms do
      terms[i].setCursorPos(termX, termY)
      terms[i].setTextColor(colors.lightGray)
      terms[i].write(".")
    end
    termX = termX + 1
    os.sleep(x)
  end
end

local pcLogError = function(...)
  local rst = pack(pcall(peripheral.call, unpack({...})))
  wait(waitThrottlePC)
  if rst[1] == false then
    print(select(2, ...) .. " failed on " .. select(1, ...), colors.red)
    return nil
  end
  return select(2, unpack(rst))
end

local pcLogErrorAndNil = function(...)
  local rst = pack(pcall(peripheral.call, unpack({...})))
  wait(waitThrottlePC)
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
      wait(waitThrottleAE)
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

    local getListQty = function(is)
      local result = 1 / 0
      for k, v in pairs(is) do
        local qty = getQty(getItem(k))
        result = math.min(result, math.floor(qty / v))
      end
      return result
    end

    local getListSum = function(is)
      local result = 0
      for _, v in pairs(is) do
        result = result + v
      end
      return result
    end

    local rankRecipes = function(recipes, adjuest)
      for _, v in pairs(recipes) do
        local i
        if v.is ~= nil then
          i = getListQty(v.is)
        else
          i = getQty(getItem(v.i))
        end
        local o = getQty(getItem(v.o))
        if i <= 0 then v.demand = -1/0
        else v.demand = (v.qty - o) / v.qty end
        if adjuest ~= nil then adjuest(v) end
      end
      table.sort(recipes, function(x, y) return x.demand > y.demand end)
    end

    local forEachSlot = function(inv, f)
      local stacks = pcLogErrorAndNil(inv, "getAllStacks")
      if stacks == nil then return false end
      for slot, item in pairs(stacks) do
        f(item.all(), slot)
      end
      return true
    end

    local forEachFluid = function(inv, dir, f)
      local info = pcLogErrorAndNil(inv, "getTankInfo", dir)
      if info == nil then return false end
      for _, v in pairs(info) do
        local fluid = v.contents
        fluid.qty = fluid.amount
        f(fluid)
      end
      return true
    end

    -- Name Conflicts
    local clayBall = idMap["minecraft:clay_ball"]
    local clayBlock = idMap["minecraft:clay"]
    local torch = idMap["minecraft:torch"]

    -- Reactor Controller

    pcLogError("left", "turnOn")

    -- CobbleGen

    do
      local inv = "tile_thermalexpansion_machine_extruder_name_5"
      local dir = "east"
      local avail = getQty(getItem("Cobblestone"))
      local toStock = 1000000
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

    -- Sludge

    local lowSludge = false
    do
      local inf = "tileinterface_29"
      local dir = "up"
      local toStockL = 16
      local toStockH = 32

      local item = getItem("Sludge Bucket")
      local inStock = getQty(item)
      if inStock < toStockL then
        print("Low Sludge", colors.yellow)
      elseif inStock > toStockH then
        print("Boiling Sludge", colors.blue)
        pushItem(inf, item, dir, 1)
      end
    end

    -- Tree farm

    do
      local harvester = "harvester_3"
      local inv = "planter_2"
      local inf = "tileinterface_3"
      local dir = "up"
      local maxInproc = 16
      local recipes = {
        {i = "Oak Sapling",     o = "Oak Sapling",     qty = 4096, shear = false},
        {i = "Rubber Sapling",  o = "Rubber Sapling",  qty = 4096, shear = false},
        {i = "Oak Sapling",     o = "Oak Wood",        qty = 4096, shear = false},
        {i = "Oak Sapling",     o = "Oak Leaves",      qty = 4096, shear = true},
        {i = "Oak Sapling",     o = "Apple",           qty = 4096, shear = false},
        {i = "Rubber Sapling",  o = "Rubber Wood",     qty = 4096, shear = false},
        {i = "Rubber Sapling",  o = "Rubber Leaves",   qty = 4096, shear = true},
        {i = "Rubber Sapling",  o = "Raw Rubber",      qty = 4096, shear = false},
        {i = "Seeds",           o = "Wheat",           qty = 4096, shear = false},
        {i = "Seeds",           o = "Seeds",           qty = 4096, shear = false},
        {i = "Carrot",          o = "Carrot",          qty = 4096, shear = false},
        {i = "Potato",          o = "Potato",          qty = 4096, shear = false},
        {i = "Industrial Hemp Seeds", o = "Industrial Hemp Seeds", qty = 4096, shear = false},
        {i = "Industrial Hemp Seeds", o = "Industrial Hemp Fiber", qty = 32768, shear = false}
      }
      rankRecipes(recipes, function(x)
        if type(x.o) == "string" and
            (string.match(x.o, "Sapling$") ~= nil
            or string.match(x.o, "Seeds$") ~= nil) then
          if x.demand > 0 then x.demand = 2 + x.demand end
        end
      end)
      if recipes[1].demand > 0 or lowSludge then
        pcLogError(harvester, "setShearLeaves", recipes[1].shear)
        local inproc = 0
        if forEachSlot(inv, function(x) inproc = inproc + x.qty end) then
          pushItem(inf, getItem(recipes[1].i), dir, maxInproc - inproc)
        end
      end
    end

    -- Pulverizer

    do
      local inf = "tileinterface_14"
      local dir = "down"
      local inv = "tile_thermalexpansion_machine_pulverizer_name_1"
      local maxInproc = 48
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
        inproc = inproc + pushItem(inf, item, dir, math.min(qty, maxInproc - inproc))
      end
      local recipes = {
        {i = "Cobblestone", o = "Sand",                qty = 4096},
        {i = "Sand",        o = "Silicon",             qty = 4096},
        {i = "Obsidian",    o = "Pulverized Obsidian", qty = 4096},
        {i = "Oak Wood",    o = "Sawdust",             qty = 4096},
        {i = "Sandstone",   o = "Niter",               qty = 4096},
        {i = "Certus Quartz Crystal", o = "Certus Quartz Dust", qty = 16},
        {i = "Nether Quartz",         o = "Nether Quartz Dust", qty = 16},
        {i = "Fluix Crystal",         o = "Fluix Dust",         qty = 16}
      }
      rankRecipes(recipes)
      for i = 1, #recipes do
        local recipe = recipes[i]
        if recipe.demand > 0 then
          inproc = inproc + pushItem(inf, getItem(recipes[i].i), dir,
            math.min(maxInproc - inproc,
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
      local maxInproc = 48
      local recipes = {
        {i = "Sand",        o = "Glass",        qty = 4096},
        {i = "Oak Wood",    o = "Charcoal",     qty = 4096},
        {i = "Cobblestone", o = "Stone",        qty = 4096},
        {i = "Raw Rubber",  o = "Rubber Bar",   qty = 4096},
        {i = "Rubber Bar",  o = "Raw Plastic",  qty = 4096},
        {i = "Wheat Flour", o = "Bread",        qty = 8},
        {i = "Bread",       o = "Toast",        qty = 64},
        {i = clayBall,      o = "Brick",        qty = 64},
        {i = "Charcoal",    o = "Graphite Bar", qty = 64}
      }
      rankRecipes(recipes)
      local inproc = 0
      local stack = pcLogError(inv, "getStackInSlot", 1)
      if stack ~= nil then inproc = stack.qty end
      for i = 1, #recipes do
        local recipe = recipes[i]
        if recipe.demand > 0 then
          inproc = inproc + pushItem(inf, getItem(recipes[i].i), dir,
            math.min(maxInproc - inproc,
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

    -- Induction Smelter (Sand)

    do
      local inv = "tile_thermalexpansion_machine_smelter_name_0"
      local inf = "tileinterface_16"
      local dir = "down"
      local maxInprocOre = 48
      local maxInprocPulverized = 48
      local sandToStock = 48
      local inproc = 0
      local sand = 0
      if forEachSlot(inv, function(x)
            if x.display_name == "Sand" then sand = sand + x.qty
            else inproc = inproc + x.qty end
          end) then
        pushItem(inf, getItem("Sand"), dir, sandToStock - sand)
        local ores = {
          "Yellorite Ore", "Silver Ore", "Lead Ore", "Tin Ore", "Uranium Ore",
          "Nether Tin Ore", "Nether Iron Ore", "Nether Copper Ore",
          "Nether Gold Ore", "Nether Lead Ore"
        }
        for _, ore in pairs(ores) do
          inproc = inproc + pushItem(inf, getItem(ore), dir, maxInprocOre - inproc)
        end
        local recipes = {
          {i = "Pulverized Iron",   o = "Iron Ingot",   qty = 256},
          {i = "Pulverized Gold",   o = "Gold Ingot",   qty = 256},
          {i = "Pulverized Copper", o = "Copper Ingot", qty = 256},
          {i = "Draconium Dust", o = "Draconium Ingot", qty = 256},
          {i = "Pulverized Ferrous Metal", o = "Ferrous Ingot", qty = 256},
        }
        rankRecipes(recipes)
        for i = 1, #recipes do
          local recipe = recipes[i]
          if recipe.demand > 0 then
            local input = getItem(recipe.i)
            local output = getItem(recipe.o)
            local toProc = math.min(getQty(input), maxInprocPulverized - inproc,
              recipe.qty - getQty(output) - inproc)
            toProc = math.floor(toProc / 2) * 2
            inproc = inproc + pushItem(inf, getItem(recipes[i].i), dir, toProc)
          end
        end
      end
    end

    -- Saw Mill

    do
      local inv = "tile_thermalexpansion_machine_sawmill_name_0"
      local inf = "tileinterface_17"
      local dir = "down"
      local maxInproc = 48
      local recipes = {
        {i = "Oak Wood",    o = "Oak Wood Planks",    qty = 4096},
        {i = "Rubber Wood", o = "Jungle Wood Planks", qty = 4096},
        {i = "Rubber Wood", o = "Raw Rubber",         qty = 4096}
      }
      rankRecipes(recipes)
      local inproc = 0
      if forEachSlot(inv, function(x)
        inproc = inproc + x.qty
      end) then
        for i = 1, #recipes do
          local recipe = recipes[i]
          if recipe.demand > 0 then
            inproc = inproc + pushItem(inf, getItem(recipe.i), dir,
              math.min(maxInproc - inproc,
                recipe.qty - inproc - getQty(getItem(recipe.o))))
          end
        end
      end
    end

    -- Paper Mill

    do
      local inv = "tile_thermalexpansion_machine_assembler_name_0"
      local inf = "tileinterface_9"
      local dir = "up"
      local maxInproc = 256
      local toStock = 4096
      local inproc = 0
      if forEachSlot(inv, function(x)
          if x.display_name == "Sawdust" then inproc = inproc + x.qty end end) then
        local toProc = math.min(maxInproc - inproc,
          toStock - inproc - getQty(getItem("Paper")))
        toProc = math.floor(toProc / 4) * 4
        pushItem(inf, getItem("Sawdust"), dir, toProc)
      end
    end

    -- Alchemical Construct

    do
      local inv = "tile_extrautils_chestfull_name_6"
      local inf = "tileinterface_18"
      local dir = "up"
      local maxInproc = 8
      local recipes = {
        {i = "Charcoal",              o = "Alumentum",             qty = 4096},
        {i = "Glowstone Dust",        o = "Glowstone Dust",        qty = 4096},
        {i = "Certus Quartz Crystal", o = "Certus Quartz Crystal", qty = 4096},
        {i = "Nether Quartz",         o = "Nether Quartz",         qty = 4096},
        {i = "Gunpowder",             o = "Gunpowder",             qty = 4096},
        {i = "Emerald Fragment",      o = "Emerald Fragment",      qty = 64},
        {i = "Slimeball",             o = "Slimeball",             qty = 4096},
        {i = "Ink Sac",               o = "Ink Sac",               qty = 4096}
      }
      rankRecipes(recipes)
      local inproc = 0
      if forEachSlot(inv, function(x) inproc = inproc + x.qty end) then
        for i = 1, #recipes do
          local recipe = recipes[i]
          if recipe.demand > 0 then
            inproc = inproc + pushItem(inf, getItem(recipe.i), dir,
              math.min(maxInproc - inproc,
                recipe.qty - inproc - getQty(getItem(recipe.o))))
          end
        end
      end
    end

    -- Essence Level

    local lowEssence = false
    local highEssence = false
    do
      local inv = "drum_0"
      local dir = "down"
      local toStockL = 1024000
      local toStockH = 65535000

      local inStock = 0
      if forEachFluid(inv, dir, function(x)
        if x.name == "mobessence" then
          inStock = inStock + x.qty
        end
      end) then
        if inStock < toStockL then
          lowEssence = true
          print("Low Mob Essence", colors.yellow)
        elseif inStock > toStockH then
          highEssence = true
          print("Too Many Mob Essence", colors.yellow)
        end
      end
    end

    -- Mob Soul Level

    local lowMobSoul = false
    do
      local toStock = 256
      local inStock = 0
      for _, v in pairs(items) do
        if v.display_name == "Mob Soul" then
          inStock = inStock + v.qty
        end
      end
      if inStock < toStock then
        print("Mob Soul: " .. inStock .. "/" .. toStock, colors.yellow)
        lowMobSoul = true
      end
    end

    -- Wisp Farm

    local maxEtherealEssence = nil
    do
      local inv = "note_block_0"
      local note = 0
      local toStock = 4096
      local inStock = 0
      for _, v in pairs(items) do
        if v.display_name == "Ethereal Essence" then
          inStock = inStock + v.qty
          if maxEtherealEssence == nil or maxEtherealEssence.qty < v.qty then
            maxEtherealEssence = v
          end
        end
      end
      if inStock < toStock or lowEssence then
        print("Ethereal Essence: " .. inStock .. "/" .. toStock, colors.yellow)
        pcLogError(inv, "setPitch", note)
        pcLogError(inv, "triggerNote")
      end
    end

    -- Enderman Farm

    do
      local inv = "note_block_0"
      local note = 1
      local toStock = 4096

      if getQty(getItem("Ender Pearl")) < toStock
          or getQty(getItem("Enderman Head")) < toStock
          or lowEssence or lowMobSoul then
        print("Enderman Farm On", colors.yellow)
        pcLogError(inv, "setPitch", note)
        pcLogError(inv, "triggerNote")
      end
    end

    -- Pink Slime Farm

    do
      local inv = "note_block_0"
      local note = 2
      local toStock = 4096

      if getQty(getItem("Pink Slimeball")) < toStock
          or highEssence or lowMobSoul then
        print("Pink Slime Farm On", colors.yellow)
        pcLogError(inv, "setPitch", note)
        pcLogError(inv, "triggerNote")
      end
    end

    -- Blaze Farm

    do
      local inv = "note_block_0"
      local note = 3
      local toStock = 4096

      if getQty(getItem("Blaze Rod")) < toStock
          or lowEssence or lowMobSoul then
        print("Blaze Farm On", colors.yellow)
        pcLogError(inv, "setPitch", note)
        pcLogError(inv, "triggerNote")
      end
    end

    -- Wither Skeleton Farm

    do
      local inv = "note_block_0"
      local note = 4
      local toStock = 4096
    
      if getQty(getItem("Bone")) < toStock
          or getQty(getItem("Necrotic Bone")) < toStock
          or getQty(getItem("Wither Skeleton Skull")) < toStock
          or lowEssence or lowMobSoul then
        print("Wither Skeleton Farm On", colors.yellow)
        pcLogError(inv, "setPitch", note)
        pcLogError(inv, "triggerNote")
      end
    end

    -- Essentia Distillation & Centrifugation

    do
      local provider = "EssentiaProvider_0"
      local furnaces = {
        {
          inv = "container_alchemyfurnace_6",
          inf = "tileinterface_22",
          dir = "east"
        }, {
          inv = "container_alchemyfurnace_7",
          inf = "tileinterface_22",
          dir = "west"
        }, {
          inv = "container_alchemyfurnace_8",
          inf = "tileinterface_23",
          dir = "east"
        }, {
          inv = "container_alchemyfurnace_9",
          inf = "tileinterface_23",
          dir = "west"
        }, {
          inv = "container_alchemyfurnace_10",
          inf = "tileinterface_24",
          dir = "east"
        }, {
          inv = "container_alchemyfurnace_11",
          inf = "tileinterface_24",
          dir = "west"
        }
      }
      local recipes = {
        {i = "Bed",          o = {Pannus = 6, Desidia = 4, Fabrico = 3}},
        {i = "Oak Leaves",   o = {Herba = 1}},
        {i = "Charcoal",     o = {Potentia = 2, Ignis = 2}},
        {i = "Paper",        o = {Cognitio = 1}},
        {i = "Obsidian",     o = {Terra = 2, Ignis = 2, Tenebrae = 1}},
        {i = "Oak Wood",     o = {Arbor = 4}},
        {i = "Rubber Bar",   o = {Motus = 2, Instrumentum = 2}},
        {i = "Raw Plastic",  o = {Fabrico = 1, Ignis = 1, Ordo = 1, Perditio = 1}},
        {i = "Glass",        o = {Vitreus = 1}},
        {i = "Silicon",      o = {Ignis = 1, Sensus = 1, Ordo = 1}},
        {i = "Sawdust",      o = {Arbor = 1, Perditio = 1}},
        {i = "Cobblestone",  o = {Terra = 1, Perditio = 1}},
        {i = "Boat",         o = {Aqua = 4, Iter = 4, Arbor = 3}},
        {i = "TNT",          o = {Perditio = 18, Ignis = 15, Terra = 3, Ira = 2}, negative = {Perditio = 20, Ignis = 20}},
        {i = "Glass Bottle", o = {Vacuos = 1}},
        {i = "Netherrack",   o = {Terra = 2, Ignis = 1, Infernus = 1}},
        {i = torch,          o = {Lux = 1}},
        {i = "Dropper",      o = {Terra = 5, Perditio = 5, Machina = 1, Permutatio = 1, Vacuos = 1, Potentia = 1}},
        {i = "Runic Glass",  o = {Praecantatio = 2, Ordo = 2, Vitreus = 1}},
        {i = "String",       o = {Bestia = 1, Pannus = 1}},
        {i = "Potato",       o = {Fames = 1, Messis = 2}},
        {i = "Slimeball",    o = {Limus = 2}, negative = {Aqua = 2, Victus = 2}},
        {i = "Blank Record", o = {Fabrico = 6, Ignis = 6, Ordo = 6, Perditio = 6, Sensus = 4, Lucrum = 4}},
        {i = "Stone Shears", o = {Meto = 2, Terra = 1, Perditio = 1}},
        {i = "Wooden Sword", o = {Arbor = 2, Telum = 1}},
        {i = "Sludge Bucket", o = {Metallum = 8, Venenum = 2, Terra = 1, Aqua = 1, Vitium = 1, Vacuos = 1}},
        {i = "Empty Syringe", o = {Metallum = 3, Instrumentum = 2, Motus = 1, Vacuos = 1, Sano = 1}},
        {i = "Stone Pickaxe", o = {Terra = 2, Perditio = 2, Perfodio = 2, Arbor = 1}},
        {i = "Pressure Plate", o = {Terra = 3, Machina = 1, Sensus = 1}},
        {i = "Obsidian Totem", o = {Terra = 4, Tenebrae = 2, Alienis = 2}},
        {i = "Wooden Chestplate", o = {Arbor = 24, Tutamen = 2}},
        {i = "Pulverized Obsidian", o = {Ignis = 2, Tenebrae = 1, Perditio = 1}}
      }
      local toStockL = 1024
      local toStockH = 3072
      local maxInproc = 32
      local centrifugeRecipes = {
        {
          computer = "computer_401",
          channel = 1,
          recipes = {
            {n = 0, i = "Motus", o1 = "Aer", o2 = "Ordo"},
            {n = 1, i = "Herba", o1 = "Victus", o2 = "Terra"},
            {n = 2, i = "Fabrico", o1 = "Instrumentum", o2 = "Humanus"},
            {n = 3, i = "Iter", o1 = "Motus", o2 = "Terra"},
            {n = 4, i = "Arbor", o1 = "Herba", o2 = "Aer"},
            {n = 5, i = "Vacuos", o1 = "Aer", o2 = "Perditio"},
            {n = 6, i = "Sensus", o1 = "Aer", o2 = "Spiritus"},
            {n = 7, i = "Tenebrae", o1 = "Lux", o2 = "Vacuos"},
            {n = 8, i = "Humanus", o1 = "Bestia", o2 = "Cognitio"},
            {n = 9, i = "Instrumentum", o1 = "Humanus", o2 = "Ordo"},
            {n = 10, i = "Victus", o1 = "Aqua", o2 = "Terra"},
            {n = 11, i = "Cognitio", o1 = "Ignis", o2 = "Spiritus"},
            {n = 12, i = "Spiritus", o1 = "Mortuus", o2 = "Victus"},
            {n = 13, i = "Bestia", o1 = "Motus", o2 = "Victus"},
            {n = 14, i = "Mortuus", o1 = "Victus", o2 = "Perditio"},
            {n = 15, i = "Pannus", o1 = "Instrumentum", o2 = "Bestia"}
          }
        }, {
          computer = "computer_404",
          channel = 2,
          recipes = {
            {n = 0, i = "Lux", o1 = "Aer", o2 = "Ignis"},
            {n = 1, i = "Desidia", o1 = "Vinculum", o2 = "Spiritus"},
            {n = 2, i = "Venenum", o1 = "Aqua", o2 = "Perditio"}
          }
        }
      }

      -- Ethereal Essence Recipe
      if maxEtherealEssence ~= nil then
        table.insert(recipes, {i = maxEtherealEssence, o = {Auram = 2}})
      end

      -- Aspects gathering
      local inStockRaw = {pcLogErrorAndNil(provider, "getAspects")}
      wait(waitThrottleAE)
      local inStock = {}
      for i = 1, (#inStockRaw/2) do
        inStock[inStockRaw[i * 2 - 1]] = inStockRaw[i * 2]
      end

      -- Add missing aspects
      for _, v in pairs(recipes) do
        for k, _ in pairs(v.o) do
          if inStock[k] == nil then inStock[k] = 0 end
        end
      end

      for _, v in pairs(centrifugeRecipes) do
        for _, v in pairs(v.recipes) do
          if inStock[v.i] == nil then inStock[v.i] = 0 end
          if inStock[v.o1] == nil then inStock[v.o1] = 0 end
          if inStock[v.o2] == nil then inStock[v.o2] = 0 end
        end
      end

      -- Ranking
      local sumSquared = function(delta, factor)
        if factor == nil then factor = 1 end
        local result = 0
        for k, v in pairs(inStock) do
          local amt = v
          if delta[k] ~= nil then amt = amt + delta[k] * factor end
          if amt > toStockH then
            result = result + math.pow(amt - toStockH, 2)
          elseif amt < toStockL then
            result = result + math.pow(amt - toStockL, 2)
          end
        end
        return result
      end

      local originalSS = sumSquared{}

      -- Distillation handling
      for _, v in pairs(recipes) do
        -- Speed Correction
        local correction = 0
        for _, v in pairs(v.o) do correction = correction + v end
        -- Delta
        local delta
        if v.negative == nil then
          delta = v.o
        else
          delta = {}
          for asp, amt in pairs(v.o) do delta[asp] = amt end
          for asp, amt in pairs(v.negative) do
            if delta[asp] ~= nil then
              delta[asp] = math.max(delta[asp] - amt, 0)
            end
          end
        end
        v.SS = sumSquared(delta, 1 / correction)
      end

      table.sort(recipes, function(x, y) return x.SS < y.SS end)

      local recipe = nil
      if getQty(getItem("Transaction Log")) > 0 then
        recipe = {i = "Transaction Log"}
      elseif getQty(getItem("Bow")) > 0 then
        recipe = {i = "Bow"}
      elseif getQty(getItem("Stone Sword")) > 0 then
        recipe = {i = "Stone Sword"}
      else
        for i = 1, #recipes do
          if getQty(getItem(recipes[i].i)) > 0 and recipes[i].SS < originalSS then
            recipe = recipes[i]
            break
          end
        end
      end

      if recipe ~= nil then
        for _, furnace in pairs(furnaces) do
          local inproc = 0
          if forEachSlot(furnace.inv, function(x, i)
                if i == 1 then inproc = inproc + x.qty end
              end) then
            pushItem(furnace.inf, getItem(recipe.i), furnace.dir,
              maxInproc - inproc, 1)
          end
        end
      end

      -- Centrifugation handling
      for _, v in pairs(centrifugeRecipes) do
        local recipes = v.recipes
        for _, v in pairs(recipes) do
          v.SS = sumSquared{[v.i] = -2, [v.o1] = 1, [v.o2] = 1}
        end
        table.sort(recipes, function(x, y) return x.SS < y.SS end)
        local output = 0
        for i = 1, #recipes do
          local recipe = recipes[i]
          if recipe.SS < originalSS then
            print(recipe.i .. " -> " .. recipe.o1 .. ", " .. recipe.o2, colors.blue)
            output = bit.bor(output, bit.blshift(1, recipe.n))
          end
        end
        pcLogError(v.computer, "turnOn")
        pcLogError(modem, "transmit", v.channel, v.channel, output)
      end
    end

    -- Prevent using-up materials

    do
      local toPreserve = {"Emerald Fragment", "Gunpowder"}

      for _, v in pairs(toPreserve) do
        local item = getItem(v)
        local qty = getQty(item)
        if qty <= 0 then
          print(v .. " not found", colors.red)
        elseif qty < 32 then
          print("Preserving " .. v, colors.yellow)
          item.qty = 0
        else
          item.qty = item.qty - 32
        end
      end
    end

    -- Crafter

    do
      local maxInproc = 64
      local crafters = {
        {
          inv = "crafter_inventory_1",
          inf = "tileinterface_25",
          dir = "east",
          recipes = {
            {is = {["Oak Wood Planks"] = 5}, o = "Boat", qty = 64},
            {is = {["Oak Wood"] = 8}, o = "Wooden Chestplate", qty = 64},
            {is = {["Glass"] = 3}, o = "Glass Bottle", qty = 64},
            {is = {["Stick"] = 1, ["Charcoal"] = 1}, o = torch, qty = 64},
            {is = {["Cobblestone"] = 7, ["Redstone"] = 1}, o = "Dropper", qty = 64},
            {is = {["Industrial Hemp Fiber"] = 3}, o = "String", qty = 64},
            {is = {["Wheat"] = 1}, o = "Wheat Flour", qty = 16},
            {is = {["Iron Ingot"] = 1, ["Stick"] = 1}, o = "Chisel", qty = 1},
          }
        }, {
          inv = "crafter_inventory_2",
          inf = "tileinterface_27",
          dir = "east",
          recipes = {
            {is = {["Oak Wood Planks"] = 2}, o = "Stick", qty = 256},
            {is = {["Stone"] = 2}, o = "Pressure Plate", qty = 64},
            {is = {["Sand"] = 4}, o = "Sandstone", qty = 64},
            {is = {["Cobblestone"] = 3, ["Stick"] = 2}, o = "Stone Pickaxe", qty = 64},
            {is = {["Raw Plastic"] = 8, ["Paper"] = 1}, o = "Blank Record", qty = 64},
            {is = {["Emerald Fragment"] = 9}, o = "Emerald", qty = 4096},
            {is = {["Obsidian"] = 4}, o = "Obsidian Tile", qty = 64},
            {is = {["Iron Ingot"] = 3}, o = "Bucket", qty = 64}
          }
        }, {
          inv = "crafter_inventory_3",
          inf = "tileinterface_28",
          dir = "east",
          recipes = {
            {is = {["Obsidian Tile"] = 4}, o = "Obsidian Totem", qty = 64},
            {is = {["Raw Plastic"] = 4}, o = "Plastic Sheets", qty = 64},
            {is = {["Cobblestone"] = 2}, o = "Stone Shears", qty = 64},
            {is = {["Plastic Sheets"] = 4, ["Iron Ingot"] = 1, ["Rubber Bar"] = 1}, o = "Empty Syringe", qty = 64},
            {is = {["Stick"] = 1, ["Oak Wood Planks"] = 2}, o = "Wooden Sword", qty = 64},
            {is = {["Gunpowder"] = 5, ["Sand"] = 4}, o = "TNT", qty = 64},
            {is = {["String"] = 4}, o = "Wool", qty = 4096}
          }
        }, {
          inv = "crafter_inventory_4",
          inf = "tileinterface_30",
          dir = "east",
          recipes = {
            {is = {["Oak Wood Planks"] = 3, ["Wool"] = 3}, o = "Bed", qty = 64}
          }
        }
      }
      for _, crafter in pairs(crafters) do
        rankRecipes(crafter.recipes)
        local inproc = 0
        if forEachSlot(crafter.inv, function(x) inproc = inproc + x.qty end) then
          for i = 1, #crafter.recipes do
            local recipe = crafter.recipes[i]
            if recipe.demand > 0 then
              local listSum = getListSum(recipe.is)
              local toProc = math.floor(math.min(getListQty(recipe.is) * listSum,
                maxInproc - inproc) / listSum)
              for k, v in pairs(recipe.is) do
                inproc = inproc + pushItem(crafter.inf, getItem(k), crafter.dir, toProc * v)
              end
            end
          end
        end
      end
    end

    -- Clay

    do
      local inv = "tile_extrautils_chestfull_name_7"
      local inf = "tileinterface_26"
      local dir = "south"
      local toStock = 256
      local maxInproc = 32

      local inproc = 0
      if forEachSlot(inv, function(x)
            if x.id == "minecraft:clay" then inproc = inproc + x.qty end
          end) then
        pushItem(inf, clayBlock, dir, math.min(maxInproc - inproc,
          toStock - inproc - getQty(clayBall)))
      end
    end

    -- Runic Glass

    do
      local inv = "autochisel_0"
      local inf = "tileinterface_14"
      local dir = "south"
      local toStock = 4096
      local maxInproc = 64

      local inproc = 0
      local hasChisel = false
      if forEachSlot(inv, function(x)
            if x.display_name == "Chisel" then hasChisel = true
            elseif x.display_name == "Glass" then inproc = inproc + x.qty
            elseif x.display_name == "Runic Glass" then inproc = inproc + x.qty end
          end) then
        if not hasChisel then pushItem(inf, getItem("Chisel"), dir, 1) end
        pushItem(inf, getItem("Glass"), dir, math.min(
          maxInproc - inproc,
          toStock - getQty(getItem("Runic Glass")) - inproc))
      end
    end

  end

  -- End of Cycle

  local timer = os.startTimer(waitCycle)
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
