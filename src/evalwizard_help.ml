let make text = object
  method permission _ () = true
  method execute _ () = [text]
end
