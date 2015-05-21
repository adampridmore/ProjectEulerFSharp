module resources

let loadResourceAsText name = 
  let r = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(name)
  
  let reader = new System.IO.StreamReader(r)
  
  reader.ReadToEnd()

