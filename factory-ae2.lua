-- by cybcaoyibo

local infMain            = "tileinterface_2"
local invCobbleGen       = "tile_extrautils_chestfull_name_0"
local dirCobbleGen       = "up"
local infPulverizer      = "tileinterface_3"
local dirPulverizer      = "down"
local infFurnace         = "tileinterface_4"
local dirFurnace         = "down"
local infSoulSand        = "tileinterface_5"
local dirSoulSand        = "down"
local infHammer          = "tileinterface_6"
local dirHammer          = "down"
local infCompressor      = "tileinterface_17"
local dirCompressor      = "down"
local infSieve           = "tileinterface_8"
local dirSieve           = "down"
local infCharger         = "tileinterface_9"
local dirCharger         = "down"
local invTreePlanter     = "planter_0"
local infTreePlanter     = "tileinterface_10"
local dirTreePlanter     = "west"
local treeHarvester      = "harvester_0"
local infDirt            = "tileinterface_13"
local dirDirt            = "down"
local infFluix           = "tileinterface_12"
local dirFluix           = "down"
local infSaw             = "tileinterface_14"
local dirSaw             = "down"
local invCropPlanter     = "planter_1"
local infCropPlanter     = "tileinterface_18"
local dirCropPlanter     = "up"
local invPureChamber     = "crafter_inventory_0"
local infPureChamber     = "tileinterface_19"
local dirPureChamberDust = "down"
local dirPureChamberSeed = "east"
local invPaper           = "tile_thermalexpansion_machine_assembler_name_0"
local infPaper           = "tileinterface_22"
local dirPaper           = "down"
local cleavers = {
  {name = "Cursed Dirt Farm", inf = "tileinterface_21", dir = "west"},
  {name = "Wither Skeleton Farm", inf = "tileinterface_24", dir = "down"}
}

local monitors = {{name = "right", scale = 0.7}}

local requestAmount = 32

local patternGravel = " Ore Gravel$"
local patternSand = " Ore Sand$"
local patternDust = " Ore Dust$"
local patternBroken = "^Broken .+ Ore$"
local patternCrushed = "^Crushed .+ Ore$"
local patternPowdered = "^Powdered .+ Ore$"
local isMetalPowder = function(name)
  if string.match(name, " Grit$") ~= nil then return true end
  if name == "Yellorium Dust" then return true end
  if name == "Signalum Blend" then return true end
  local result = string.match(name, "^Pulverized (.*)$")
  if result ~= nil and result ~= "Coal"
        and result ~= "Charcoal" and result ~= "Obsidian"
        then
    return true
  end
  return false
end
local patternCircuit = "^Printed .+ Circuit$"

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
      local result = pcLogErrorAndNil(me, "exportItem", item.identifier, direction, qty, intoSlot)
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

    -- Ambiguous Items

    local itemDirt = nil
    local itemSulfur = nil
    for _, item in pairs(items) do
      if item.id == "minecraft:dirt" and item.dmg == 0 then
        itemDirt = item
      elseif item.id == "ThermalFoundation:material" and item.display_name == "Sulfur" then
        itemSulfur = item
      end
    end

    -- Cleavers

    for _, cfg in pairs(cleavers) do
      local rawItems = pcLogErrorAndNil(cfg.inf, "getAvailableItems", "all")
      if rawItems ~= nil then
        local rawItemCleaver = nil
        local dupCleaver = false
        for _, v in pairs(rawItems) do
          if rawItemCleaver == nil then
            rawItemCleaver = v
          else
            dupCleaver = true
            break
          end
        end
        if dupCleaver then
          print("More than 1 item in " .. cfg.name, colors.red)
        elseif rawItemCleaver == nil then
          print("No item in " .. cfg.name, colors.orange)
        else
          local itemCleaver = rawItemCleaver.item
          if itemCleaver == nil then
            print(cfg.name .. " item doesn't have detail", colors.red)
          elseif itemCleaver.health_bar == nil then
            print(cfg.name .. " item doesn't have damage bar", colors.orange)
          else
            local percentage = (1 - itemCleaver.health_bar) * 100
            print(cfg.name .. " cleaver damage: " .. percentage .. "%", colors.blue)
            if percentage < 50 then
              print("Repairing cleaver in " .. cfg.name)
              local result = pcLogErrorAndNil(cfg.inf, "exportItem", rawItemCleaver.fingerprint, cfg.dir, 1)
              if result ~= nil then
                if result.size ~= 1 then
                  print(cfg.name .. "cleaver repair export result size != 1", colors.red)
                end
              end
            end
          end
        end
      end
    end

    -- CobbleGen

    do
      local avail = getQty(nameMap["Cobblestone"])
      local toStock = 4096
      if avail < toStock then
        local stacks = pcLogErrorAndNil(invCobbleGen, "getAllStacks")
        if stacks ~= nil then
          local done = 0
          for slot, v in pairs(stacks) do
            local result = pcLogErrorAndNil(invCobbleGen, "pushItem", dirCobbleGen, slot, toStock - avail - done)
            if result ~= nil then
              done = done + result
              if toStock - avail - done == 0 then break end
            end
          end
          print("CobbleGen: " .. done .. "/" .. (toStock - avail))
        end
      end
    end

    -- Pulverizer

    do
      for _, item in pairs(items) do
        if item.qty > 0 and string.match(item.display_name, patternDust) ~= nil then
          pushItem(infPulverizer, item, dirPulverizer, item.qty)
        end
      end
      local makings = {
        {input = nameMap["Cobblestone"],   stock = getQty(nameMap["Gravel"])             / 4096 },
        {input = nameMap["Gravel"],        stock = getQty(nameMap["Sand"])               / 4096 },
        {input = nameMap["Sand"],          stock = getQty(nameMap["Dust"])               / 4096 },
        {input = nameMap["Nether Quartz"], stock = getQty(nameMap["Nether Quartz Dust"]) / 4096 },
        {input = nameMap["Fluix Crystal"], stock = getQty(nameMap["Fluix Dust"])         / 4096 },
        {input = nameMap["Oak Wood"],      stock = getQty(nameMap["Sawdust"])            / 4096 },
        {input = nameMap["Coal"],          stock = getQty(itemSulfur)                    / 256  }
      }
      table.sort(makings, function(x, y) return x.stock < y.stock end)
      for _, making in ipairs(makings) do
        if making.stock >= 1 then break end
        if pushItem(infPulverizer, making.input, dirPulverizer, requestAmount) > 0 then break end
      end
    end

    -- Furnace

    do
      do
        local makings = {}
        for _, item in pairs(items) do
          if item.qty > 0 then
            if isMetalPowder(item.display_name)
                or string.match(item.display_name, patternCircuit) ~= nil then
              table.insert(makings, item)
            end
          end
        end
        table.sort(makings, function(x, y) return x.qty > y.qty end)
        for _, making in ipairs(makings) do
          pushItem(infFurnace, making, dirFurnace, making.qty)
        end
      end
      local getSiliconInput = function()
        local possible = {
          {item = nameMap["Certus Quartz Dust"]},
          {item = nameMap["Nether Quartz Dust"]},
          {item = nameMap["Sky Stone Dust"]}
        }
        table.sort(possible, function(x, y) return getQty(x.item) > getQty(y.item) end)
        return possible[1].item
      end
      local makings = {
        {input = nameMap["Sand"],        stock = getQty(nameMap["Glass"])         / 4096 },
        {input = nameMap["Cobblestone"], stock = getQty(idMap["minecraft:stone"]) / 4096 },
        {input = getSiliconInput,        stock = getQty(nameMap["Silicon"])       / 4096 },
        {input = nameMap["Raw Rubber"],  stock = getQty(nameMap["Rubber Bar"])    / 4096 },
        {input = nameMap["Rubber Bar"],  stock = getQty(nameMap["Raw Plastic"])   / 4096 },
        {input = nameMap["Charcoal"],    stock = getQty(nameMap["Graphite Bar"])  / 4096 },
        {input = nameMap["Oak Wood"],    stock = getQty(nameMap["Charcoal"])      / 4096 }
      }
      table.sort(makings, function(x, y) return x.stock < y.stock end)
      for _, making in ipairs(makings) do
        if making.stock >= 1 then break end
        local input = makings[1].input
        if type(input) == "function" then input = input() end
        if pushItem(infFurnace, input, dirFurnace, requestAmount) > 0 then break end
      end
    end

    -- Soul Sand

    if getQty(nameMap["Soul Sand"]) < 4096 then
      pushItem(infSoulSand, nameMap["Sand"], dirSoulSand, requestAmount)
    end

    -- Hammer

    do
      local makings = {}
      for _, item in pairs(items) do
        if item.qty > 0 then
          if string.match(item.display_name, patternGravel) ~= nil
              or string.match(item.display_name, patternSand) ~= nil
              then
            table.insert(makings, item)
          end
        end
      end
      table.sort(makings, function(x, y) return x.qty > y.qty end)
      if #makings > 0 then
        pushItem(infHammer, makings[1], dirHammer, makings[1].qty)
      end
    end

    -- Compressor

    do
      local makings = {}
      for _, item in pairs(items) do
        if item.qty >= 4 then
          if item.id == "exnihilo:stone"
              or string.match(item.display_name, patternBroken) ~= nil
              or string.match(item.display_name, patternCrushed) ~= nil
              or string.match(item.display_name, patternPowdered) ~= nil
              then
            local qty = math.floor(item.qty / 4) * 4
            table.insert(makings, {item = item, qty = qty})
          end
        end
      end
      table.sort(makings, function(x, y) return x.qty > y.qty end)
      for _, v in pairs(makings) do
        if pushItem(infCompressor, v.item, dirCompressor, v.qty) < v.qty then break end
      end
    end

    -- Sieve

    do
      local makings = {
        {input = nameMap["Dust"],      stock = getQty(nameMap["Redstone"])              / 4096 },
        {input = nameMap["Sand"],      stock = getQty(nameMap["Yellorium Ingot"])       / 256  },
        {input = nameMap["Sand"],      stock = getQty(nameMap["Certus Quartz Crystal"]) / 4096 },
        {input = nameMap["Gravel"],    stock = getQty(nameMap["Diamond"])               / 256  },
        {input = nameMap["Gravel"],    stock = getQty(nameMap["Coal"])                  / 4096 },
        {input = nameMap["Gravel"],    stock = getQty(nameMap["Shiny Ingot"])           / 4096 },
        {input = nameMap["Soul Sand"], stock = getQty(nameMap["Nether Quartz"])         / 4096 },
        {input = itemDirt,             stock = getQty(nameMap["Grass Seeds"])           / 256  }
      }
      table.sort(makings, function(x, y) return x.stock < y.stock end)
      if makings[1].stock < 1 then
        pushItem(infSieve, makings[1].input, dirSieve, requestAmount)
      end
    end

    -- Charger

    if getQty(nameMap["Charged Certus Quartz Crystal"]) < 256 then
      pushItem(infCharger, nameMap["Certus Quartz Crystal"], dirCharger, requestAmount)
    end

    -- Tree Farm

    do
      local makings
      if getQty(nameMap["Oak Sapling"]) < 256 then
        makings = {
          {input = nameMap["Oak Sapling"],    shearLeaves = false      , stock = getQty(nameMap["Oak Sapling"])    / 256  }
        }
      elseif getQty(nameMap["Rubber Sapling"]) < 256 then
        makings = {
          {input = nameMap["Rubber Sapling"], shearLeaves = false      , stock = getQty(nameMap["Rubber Sapling"]) / 256  }
        }
      else
        makings = {
          {input = nameMap["Oak Sapling"],    shearLeaves = true       , stock = getQty(nameMap["Oak Leaves"])     / 4096 },
          {input = nameMap["Oak Sapling"],    shearLeaves = true       , stock = getQty(nameMap["Oak Wood"])       / 4096 },
          {input = nameMap["Oak Sapling"],    shearLeaves = false      , stock = getQty(nameMap["Apple"])          / 256  },
          {input = nameMap["Rubber Sapling"], shearLeaves = true       , stock = getQty(nameMap["Raw Rubber"])     / 4096 }
        }
      end
      table.sort(makings, function(x, y) return x.stock < y.stock end)
      if makings[1].stock < 1 then
        pcLogError(treeHarvester, "setShearLeaves", makings[1].shearLeaves)
        local stacks = pcLogErrorAndNil(invTreePlanter, "getAllStacks")
        if stacks ~= nil then
          local avail = 0
          for _, v in pairs(stacks) do
            avail = avail + v.basic().qty
          end
          pushItem(infTreePlanter, makings[1].input, dirTreePlanter, 8 - avail)
        end
      end
    end

    -- Crop Farm

    do
      local makings = {
        {input = nameMap["Seeds"], stock = getQty(nameMap["Wheat"]) / 4096 }
      }
      table.sort(makings, function(x, y) return x.stock < y.stock end)
      if makings[1].stock < 1 then
        local stacks = pcLogErrorAndNil(invCropPlanter, "getAllStacks")
        if stacks ~= nil then
          local avail = 0
          for _, v in pairs(stacks) do
            avail = avail + v.basic().qty
          end
          pushItem(infCropPlanter, makings[1].input, dirCropPlanter, 8 - avail)
        end
      end
    end

    -- Dirt

    do
      if getQty(itemDirt) < 4096 then
        local leaves = {
          {item = nameMap["Oak Leaves"]},
          {item = nameMap["Rubber Leaves"]}
        }
        table.sort(leaves, function(x, y) return getQty(x.item) > getQty(y.item) end)
        pushItem(infDirt, leaves[1].item, dirDirt, requestAmount)
      end
    end

    -- Fluix

    do
      if getQty(nameMap["Fluix Crystal"]) < 4096 then
        local input1 = nameMap["Redstone"]
        local input2 = nameMap["Nether Quartz"]
        local input3 = nameMap["Charged Certus Quartz Crystal"]
        local qty = math.min(getQty(input1), getQty(input2), getQty(input3), requestAmount)
        pushItem(infFluix, input1, dirFluix, qty)
        pushItem(infFluix, input2, dirFluix, qty)
        pushItem(infFluix, input3, dirFluix, qty)
      end
    end

    -- Saw

    do
      if getQty(nameMap["Oak Wood Planks"]) < 4096 then
        pushItem(infSaw, nameMap["Oak Wood"], dirSaw, requestAmount)
      end
    end

    -- Pure Chamber (Seed to Pure Crystal)

    do
      local makings = {
        {item = nameMap["Certus Quartz Seed"]},
        {item = nameMap["Nether Quartz Seed"]},
        {item = nameMap["Fluix Seed"]}
      }
      table.sort(makings, function(x, y) return getQty(x.item) > getQty(y.item) end)
      pushItem(infPureChamber, makings[1].item, dirPureChamberSeed, getQty(makings[1].item))
    end

    -- Pure Chamber (Dust to Seed)

    do
      local makings = {
          {input = nameMap["Certus Quartz Dust"], stock = getQty(nameMap["Pure Certus Quartz Crystal"]) / 4096 },
          {input = nameMap["Nether Quartz Dust"], stock = getQty(nameMap["Pure Nether Quartz Crystal"]) / 4096 },
          {input = nameMap["Fluix Dust"],         stock = getQty(nameMap["Pure Fluix Crystal"])         / 4096 }
      }
      table.sort(makings, function(x, y) return x.stock < y.stock end)
      if makings[1].stock < 1 then
        local itemSand = nameMap["Sand"]
        local amount = math.min(getQty(itemSand), getQty(makings[1].input), requestAmount)
        if amount > 0 then
          local nSlots = pcLogErrorAndNil(invPureChamber, "getInventorySize")
          if nSlots ~= nil then
            local stacks = pcLogErrorAndNil(invPureChamber, "getAllStacks")
            if stacks ~= nil then
              for _, _ in pairs(stacks) do nSlots = nSlots - 1 end
              nSlots = nSlots - 20
              print("Pure Crystal Crafter: " .. nSlots .. " free slots", colors.blue)
              if nSlots > 0 then
                pushItem(infPureChamber, makings[1].input, dirPureChamberDust, amount)
                pushItem(infPureChamber, itemSand, dirPureChamberDust, amount)
              end
            end
          end
        end
      end
    end

    -- Paper

    do
      if getQty(nameMap["Paper"]) < 4096 then
        local itemSawDust = nameMap["Sawdust"]
        local amount = math.min(math.floor(getQty(itemSawDust) / 4), requestAmount) * 4
        if amount > 0 then
          local nSlots = pcLogErrorAndNil(invPaper, "getInventorySize")
          if nSlots ~= nil then
            local stacks = pcLogErrorAndNil(invPaper, "getAllStacks")
            if stacks ~= nil then
              for _, _ in pairs(stacks) do nSlots = nSlots - 1 end
              nSlots = nSlots - 10
              print("Paper Crafter: " .. nSlots .. " free slots", colors.blue)
              if nSlots > 0 then
                pushItem(infPaper, itemSawDust, dirPaper, amount)
              end
            end
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
