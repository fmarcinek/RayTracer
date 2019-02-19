## Ray Tracer written in Haskell  

Project for studies

Aby skompilować program, należy użyć poniższej komendy w terminalu:

    ghc --make Main.hs -o RayTracer

Aby usunąć z katalogu pliki utworzone podczas kompilacji, można użyć tej komendy:

    rm *.o *.hi RayTracer

Do skompilowanego programu możemy podać dwa argumenty (będące ścieżkami odpowiednio do
    pliku źródłowego ze sceną oraz pliku, w którym chcemy zapisać obrazek - preferowany
    format to .png):

    ./RayTracer "example.ray" "picture.png"

Możemy również podać tylko jeden argument, którym będzie ścieżka do pliku
    ze sceną - wtedy obrazek tylko się wyświetli (bez zapisania):

    ./RayTracer "example.ray"
