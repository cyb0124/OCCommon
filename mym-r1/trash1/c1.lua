local waitUntil = function(t1)
  while true do
    local td = t1 - computer.uptime()
    if td <= 0 then break end
    computer.pullSignal(td)
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

local me = component.proxy(component.list("me_interface")())
local inv = component.proxy(component.list("transposer")())
local dbAddr = component.list("database")()
local db = component.proxy(dbAddr)

while true do
  local t0 = computer.uptime()
  local items = me.getItemsInNetwork()
  local nameMap = {}
  for i = 1, #items do
    local item = items[i]
    nameMap[item.name] = item
  end
  local getQty = function(x)
    if not x then return 0 end
    return x.size
  end
  local forEachSlot = function(side, f)
    local stacks = inv.getAllStacks(side)
    for i = 1, stacks.count() do
      local stack = stacks()
      if stack.name then f(stack, i) end
    end
  end

  local sideME = sides.up
  local sideM1 = sides.east
  local maxInproc = 8
  local recipes = {
    {i = "minecraft:cobblestone", o = "minecraft:sand", qty = 4096}
  }

  me.setInterfaceConfiguration(1)
  inv.transferItem(sideM1, sideME, 64, 2, 9)
  inv.transferItem(sideM1, sideME, 64, 3, 9)

  local inproc = 0
  local inprocName
  forEachSlot(sides.east, function(item, slot)
    inproc = inproc + item.size
    if slot == 1 then inprocName = item.name end
  end)
  if inproc < maxInproc then
    for _, recipe in pairs(recipes) do
      if getQty(nameMap[recipe.i]) <= 0 or inprocName and inprocName ~= recipe.i then
        recipe.demand = 0
      else
        recipe.demand = 1 - getQty(nameMap[recipe.o]) / recipe.qty
      end
    end
    table.sort(recipes, function(x, y) return x.demand > y.demand end)
    local recipe = recipes[1]
    if recipe.demand > 0 then
      local qty = math.min(maxInproc - inproc,
        recipe.qty - getQty(nameMap[recipe.o]) - inproc)
      db.clear(1)
      me.store(nameMap[recipe.i], dbAddr, 1, 1)
      me.setInterfaceConfiguration(1, dbAddr, 1, qty)
      computer.beep(440)
      inv.transferItem(sideME, sideM1, qty, 1, 1)
    end
  end

  waitUntil(t0 + 1)
end
