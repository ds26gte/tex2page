-- Last modified 2018-06-28
-- Dorai Sitaram

function hsl360_to_rgb(H, s, L)
  local c = (1 - math.abs(2*L - 1)) * s
  local x = c * (1 - math.abs(((H/60) % 2) - 1))
  local m = L - c/2
  local r1, g1, b1 = 0, 0, 0
  if H < 60 then r1 = c; g1 = x
  elseif H < 120 then r1 = x; g1 = c
  elseif H < 180 then g1 = c; b1 = x
  elseif H < 240 then g1 = x; b1 = c
  elseif H < 300 then r1 = x; b1 = c
  else r1 = c; b1 = x
  end
  return "rgb", { r1+m, g1+m, b1+m }
end

function hsl_to_rgb(h, s, L)
  return hsl360_to_rgb(h * 360, s, L)
end

function hsb360_to_rgb(h, s, b)
  local L = .5 * b * (2 - s)
  local s2 =  (b * s) / (1 - math.abs(2*L - 1))
  return hsl360_to_rgb(h, s2, L)
end

function hsb_to_rgb(h, s, b)
  return hsb360_to_rgb(h*360, s, b)
end

function hsb240_to_rgb(h, s, b)
  return hsb_to_rgb(h/240, s/240, b/240)
end

function cmy_to_rgb(c, m, y)
  return "rgb", { 1-c, 1-m, 1-y }
end

function html_to_rgb(xn)
  local y = xn
  local r = math.floor(y / 0x10000)
  y = y % 0x10000
  local g = math.floor(y / 0x100)
  local b = y % 0x100
  return "rgb", { r/255, g/255, b/255 }
end

function gray15_to_gray(g)
  return "gray", { g/15 }
end

function wavelength_to_rgb(w)
  local h
  if w <= 362.857 then h = 5
  elseif w < 440 then h = 4 + (w - 440)/-60
  elseif w < 490 then h = 4 - (w - 440)/50
  elseif w < 510 then h = 2 + (w - 510)/-20
  elseif w < 580 then h = 2 - (w - 510)/70
  elseif w < 645 then h = (w - 645)/-65
  else h = 0
  end
  h = h * 1/6
  local b = 0
  if w <= 362.857 then b = 0
  elseif w < 420 then b = .3 + .7 * (w - 380)/40
  elseif w <= 700 then b = 1
  elseif w < 814.285 then b = .3 + .7 * (w - 780)/-80
  else b = 0
  end
  return hsb360_to_rgb(h*360, 1, b)
end

function convert_to_known_color_model(model, a)
  if model == 'hsb' then
    return hsb_to_rgb(a[1], a[2], a[3])
  elseif model == 'Hsb' then
    return hsb360_to_rgb(a[1], a[2], a[3])
  elseif model == 'HSB' then
    return hsb240_to_rgb(a[1], a[2], a[3])
  elseif model == 'hsl' then
    return hsl_to_rgb(a[1], a[2], a[3])
  elseif model == 'Hsl' then
    return hsl360_to_rgb(a[1], a[2], a[3])
  elseif model == 'cmy' then
    return cmy_to_rgb(a[1], a[2], a[3])
  elseif model == 'HTML' then
    return html_to_rgb(a[1])
  elseif model == 'Gray' then
    return gray15_to_gray(a[1])
  elseif model == 'wave' then
    return wavelength_to_rgb(a[1])
  else
    return model, a
  end
end

function convert_to_csv(a)
  local s = '{'
  for i = 1, #a do
    if i > 1 then
      s = s .. ', '
    end
    s = s .. string.format("%.4f", a[i])
  end
  s = s .. '}'
  return s
end 
