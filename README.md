# Vérification et preuve automatique d’appartenance d’un mot à une grammaire

The project goal was to make an algorithm which decides wether or not a word is within a formal grammar. It is known to be a NP-hard problem but an heuristic approach (neither measured nor proved) was used.

The project was in the context of a TIPE (Travail d'Initiative Personnelle Encadrée), to be presented in the entrance exam of the ENS in 2022. It recieved a grade of 18/20.

Here is the report https://gitlab.com/ulysse_durand/preuves-grammaires/-/blob/master/readme/TIPE_Ulysse_DURAND_merged.pdf

Here are the slides https://gitlab.com/ulysse_durand/preuves-grammaires/-/blob/master/readme/diapo.pdf?ref_type=heads

The OCaml code can be found in the `programme` folder.

## Run an API with this code

You can run
```sh
docker run \
    --privileged \
    --rm \
    -p 8000:8000 \
    -v $PWD/examples:/app/examples \
    -e IMAGE=registry.gitlab.com/ulysse_durand/preuves-grammaires/ocamlrun \
    registry.gitlab.com/ulysse_durand/gitlabci-templates/str2str-api
```

To run an API executing this code
[str2str-api](gitlab.com/ulysse_durand/gitlabci-templates/str2str-api)