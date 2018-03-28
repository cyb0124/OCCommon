local modem = peripheral.wrap("right")
while true do
  modem.open(2)
  local ev, eArg, eArg2, eArg3, eArg4 = os.pullEvent()
  if ev == "key" then
    if eArg == keys.x then
      return
    end
  elseif ev == "modem_message" then
    if eArg2 == 2 then
      print(os.time() .. ": " .. eArg4)
      rs.setBundledOutput("left", eArg4)
    end
  end
end
