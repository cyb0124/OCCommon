load((function()
  local content = ""
  for chunk in component.invoke(component.list("internet")(), "request", "https://cyb1.net/oc-scripts/mym-e2e1.lua").read do
    content = content .. chunk
  end
  return content
end)())()
