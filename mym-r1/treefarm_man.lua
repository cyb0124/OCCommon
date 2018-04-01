local resolve = function(short)
  for addr in component.list() do
    if string.lower(string.sub(addr, 1, #short)) == string.lower(short) then
      return addr
    end
  end
end

local waitUntil = function(t1)
  while true do
    local td = t1 - computer.uptime()
    if td <= 0 then break end
    computer.pullSignal(td)
  end
end

local sleep = function(duration)
  waitUntil(computer.uptime() + duration)
end

local getEnergy = function()
  return computer.energy() / computer.maxEnergy()
end

local retrying = function(...)
  while true do
    local result = {({...})[1](select(2, ...))}
    if result[1] then
      return table.unpack(result)
    end
    computer.beep(880)
  end
end

local sides = {
  bottom = 0, down  = 0, yn = 0,
  top    = 1, up    = 1, yp = 1,
  back   = 2, north = 2, zn = 2,
  front  = 3, south = 3, zp = 3,
  right  = 4, west  = 4, xn = 4,
  left   = 5, east  = 5, xp = 5
}

local inf = component.proxy(component.list("me_interface")())
local dbAddr = component.list("database")()
local db = component.proxy(dbAddr)

local recipes = {
  {i = "Oak Sapling",           o = "Oak Sapling",           qty = 4096},
  {i = "Oak Sapling",           o = "Oak Wood",              qty = 4096},
  {i = "Oak Sapling",           o = "Apple",                 qty = 4096},
  {i = "Spruce Sapling",        o = "Spruce Sapling",        qty = 4096},
  {i = "Spruce Sapling",        o = "Spruce Wood",           qty = 4096},
  {i = "Seeds",                 o = "Seeds",                 qty = 4096},
  {i = "Seeds",                 o = "Wheat",                 qty = 4096},
  {i = "Carrot",                o = "Carrot",                qty = 4096},
  {i = "Potato",                o = "Potato",                qty = 4096},
  {i = "Industrial Hemp Seeds", o = "Industrial Hemp Seeds", qty = 4096},
  {i = "Industrial Hemp Seeds", o = "Industrial Hemp Fiber", qty = 16384}
}

while true do
  local t0 = computer.uptime()

  inf.setInterfaceConfiguration(1)
  inf.setInterfaceConfiguration(2)

  local items = inf.getItemsInNetwork()
  local labelMap = {}
  for i = 1, #items do
    local item = items[i]
    labelMap[item.label] = item
  end

  local getQtyForLabel = function(label)
    local item = labelMap[label]
    if item == nil then
      return 0
    else
      return item.size
    end
  end

  for _, recipe in pairs(recipes) do
    local i = getQtyForLabel(recipe.i)
    local o = getQtyForLabel(recipe.o)
    if i <= 0 then
      recipe.demand = -1/0
    else
      recipe.demand = (recipe.qty - o) / recipe.qty
    end
  end

  table.sort(recipes, function(x, y) return x.demand > y.demand end)
  local recipe = recipes[1]
  if recipe.demand > 0 then
    inf.store(labelMap[recipe.i], dbAddr, 1)
    inf.setInterfaceConfiguration(1, dbAddr, 1, 2)
  end

  local fert = labelMap["Fertilizer"]
  if fert then
    inf.store(fert, dbAddr, 1)
    inf.setInterfaceConfiguration(2, dbAddr, 1, 2)
  end

  waitUntil(t0 + 1)
end
