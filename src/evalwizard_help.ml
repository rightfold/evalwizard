let make url = object
  method permission _ () = true
  method execute _ () = [url]
end
