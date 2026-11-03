-- Make Word tables fit the page: set column widths in proportion to content
-- (so each table spans the full text width) and shrink the font as the number
-- of columns grows.

local function esc(s)
  return (s:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;"))
end

local function cell_text(cell)
  return pandoc.utils.stringify(pandoc.Pandoc(cell.contents))
end

local function widest_word(s)
  local m = 0
  for w in s:gmatch("%S+") do
    if #w > m then m = #w end
  end
  return m
end

function Table(tbl)
  if not FORMAT:match("docx") then return nil end

  local ncol = #tbl.colspecs
  if ncol == 0 then return nil end

  -- Column weight = longest cell (capped), but never narrower than the longest single word
  local weight = {}
  for i = 1, ncol do weight[i] = 4 end
  local function scan(rows)
    for _, row in ipairs(rows) do
      for i, cell in ipairs(row.cells) do
        if weight[i] then
          local t = cell_text(cell)
          local w = math.min(#t, 28)
          w = math.max(w, math.min(widest_word(t), 28))
          if w > weight[i] then weight[i] = w end
        end
      end
    end
  end
  scan(tbl.head.rows)
  for _, body in ipairs(tbl.bodies) do scan(body.body) end

  local total = 0
  for i = 1, ncol do total = total + weight[i] end
  for i = 1, ncol do
    tbl.colspecs[i] = { tbl.colspecs[i][1], weight[i] / total }
  end

  -- Font size in half-points
  local sz
  if ncol >= 12 then sz = 13
  elseif ncol >= 9 then sz = 15
  elseif ncol >= 6 then sz = 17
  else sz = 19 end
  local rpr = '<w:rPr><w:sz w:val="' .. sz .. '"/><w:szCs w:val="' .. sz .. '"/></w:rPr>'

  local function run(text)
    return pandoc.RawInline("openxml",
      '<w:r>' .. rpr .. '<w:t xml:space="preserve">' .. esc(text) .. '</w:t></w:r>')
  end

  local f = {
    Str = function(s) return run(s.text) end,
    Space = function() return run(" ") end,
    SoftBreak = function() return run(" ") end,
  }
  local function shrink(rows)
    for _, row in ipairs(rows) do
      for _, cell in ipairs(row.cells) do
        cell.contents = pandoc.Blocks(cell.contents):walk(f)
      end
    end
  end
  shrink(tbl.head.rows)
  for _, body in ipairs(tbl.bodies) do shrink(body.body) end
  return tbl
end
