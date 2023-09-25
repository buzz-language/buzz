local width = tonumber(arg and arg[1]) or 80
local height = tonumber(arg and arg[2]) or 60

local cells = {}

for _ = 1, width * height do
    table.insert(cells, math.random(5) == 1)
end

for y = 1, height do
    for x = 1, width do
        cells[y * width + x] = not cells[y * width + x];
    end
end
