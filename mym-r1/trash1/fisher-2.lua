local inv = component.proxy(component.list("transposer")())
local sideIn = 3
local sideOut = 2
local sideCrafter = 4

local repairs = {
  ["minecraft:fishing_rod"] = true,
  ["minecraft:leather_boots"] = true,
  ["minecraft:bow"] = true
}

local unpack = function(tab)
  if table.unpack ~= nil then return table.unpack(tab) end
  return _G.unpack(tab)
end

local sleep = function(duration)
  local t0 = computer.uptime()
  local t1 = t0 + duration
  while true do
    local td = t1 - computer.uptime()
    if td <= 0 then break end
    computer.pullSignal(td)
  end
end

while true do
  local stacks = inv.getAllStacks(sideIn)
  if stacks ~= nil then
    local repairMap = {}
    for slot = 1, stacks.count() do
      local stack = stacks()
      if not repairs[stack.name] or stack.damage == 0 then
        inv.transferItem(sideIn, sideOut, stack.size, slot, 1)
      else
        local otherSlot = repairMap[stack.name]
        if otherSlot then
          while 0 == inv.transferItem(sideIn, sideCrafter, 1, otherSlot, 1) do
            computer.beep(880) sleep(0.5)
          end
          while 0 == inv.transferItem(sideIn, sideCrafter, 1, slot, 2) do
            computer.beep(880) sleep(0.5)
          end
          repairMap[stack.name] = nil
          computer.beep(440)
        else
          repairMap[stack.name] = slot
        end
      end
    end
  end
  sleep(1)
end
