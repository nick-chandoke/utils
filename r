#!/usr/bin/env lua
-- "run." more convenient & cleaner version of nohup
-- TODO: make work with discord. `nohup discord &>/dev/null &` works fine but `r discord` does not, i think b/c of smth like io redirection
for i,e in pairs(arg) do arg[i]='"'..string.gsub(e,'"','\\"')..'"' end
io.popen('nohup '..table.concat(arg,' '))
os.exit()
