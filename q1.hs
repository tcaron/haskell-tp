import System.IO

begin = "<svg xmlns='http://www.w3.org/2000/svg' width='500' height='500' version='1.1'>"
end = "</svg>"
file = "image.svg"
main = do
  writeFile file begin
  appendFile file end
