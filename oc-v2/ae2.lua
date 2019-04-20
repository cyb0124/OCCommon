local dbAddr = resolve("database")
local db = component.proxy(dbAddr)
local me = component.proxy(resolve("me_interface"))
local sideBus = ???
local sideME = ???

-- Register items
for _, item in ipairs(me.getItemsInNetwork()) do labelMap[item.label] = item end

-- Extract(item, slot, toproc)
db.clear(1)
me.store({label = item.label, name = item.name}, dbAddr, 1, 1)
me.setInterfaceConfiguration(1, dbAddr, 1, toproc)
inv.transferItem(sideME, sideBus, toproc, 1, slot)
me.setInterfaceConfiguration(1)

-- Insert
inv.transferItem(sideBus, sideME, item.size, slot, 9)
