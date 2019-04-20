local modem = peripheral.wrap("left")
while true do
  modem.open(1)
  local ev, eArg, eArg2, eArg3, eArg4 = os.pullEvent()
  if ev == "key" then
    if eArg == keys.x then
      return
    end
  elseif ev == "modem_message" then
    if eArg2 == 1 then
      print(os.time() .. ": " .. eArg4)
      rs.setBundledOutput("right", eArg4)
    end
  end
end
