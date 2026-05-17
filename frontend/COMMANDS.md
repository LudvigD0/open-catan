
# Användbara Kommandon

## Nedan är lite kommandon på workflow för att komma igång med frontend
* Ställ dig i /frontend, sedan skriv:

make build
make serve


## Nedan är den äldre metoden man använde för att komma igång med frontend

- nix --extra-experimental-features "nix-command flakes" develop .#wasm
- make repl

* Sedan öppna url som skrivs ut
- main
* Sedan kan :r användas precis som vanligt när du har gjort ändringar som du vill ska visas i browser.
* Tänk även på att inte ladda om sidan flera gånger för snabbt för då brukar allt hänga sig.