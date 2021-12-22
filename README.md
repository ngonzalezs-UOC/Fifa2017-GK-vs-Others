# Fifa2017-GK-vs-Others

![img.png](aux/Fifa2017.jpg)

## Context

Els elements d'aquest repositori constitueixen la resposta a la pràctica de *Neteja i anàlisi de les dades* (pràctica 2) de l'assignatura *Tipologia i Cicle de vida de les dades* del *Màster en Ciència de Dades* de la [Universitat Oberta de Catalunya](https://www.uoc.edu/portal/ca/index.html) (UOC), corresponent al primer semestre del curs 2021-2022.

A partir del dataset [Complete FIFA 2017 Player dataset (Global)](https://www.kaggle.com/artimous/complete-fifa-2017-player-dataset-global), disponible a la plataforma [Kaggle](https://www.kaggle.com/), l'objectiu consisteix a realitzar les tasques pròpies de preprocessat de dades per després elaborar una anàlisi que respongui les següents preguntes d'investigació:

1. La característica que està més relacionada amb la valoració, és la mateixa per als porters i que per als jugadors de camp?  
2. Podem afirmar que la mitjana de l’IMC de porters i jugadors de camp és igual?  
3. Quina combinació de característiques esportives explica millor la valoració d’un jugador? Com afecta a la valoració ser o no porter?  
4. Quina és la probabilitat de ser internacional en funció de la valoració, l’edat i la posició? En quin percentatge augmenta o disminueix aquesta probabilitat el fet de ser o no porter?

## Vídeo

D'acord a l'enunciat de la pràctica, es facilita un vídeo explicatiu del projecte. Es pot accedir clicant aquí.

## Repositori 
```bash
────Fifa2017-GK-vs-Others
    │
    ├───LICENSE
    ├───README.md
    │
    ├───aux
    │       *.*
    │
    ├───data
    │       Fifa2017_original.csv
    │       Fifa2017_final.csv
    │
    ├───pdf
    │       Fifa2017-GK-vs-Others.pdf
    │
    ├───src
    │       Fifa2017-GK-vs-Others.Rmd
    │       Fifa2017-GK-vs-Others.R
    │    
    └───video
            Fifa2017-GK-vs-Others.mp4
```
- **LICENSE**: Fitxer amb els termes de la llicència aplicada al projecte.
- **README.md**: Fitxer explicatiu del projecte.
- **aux/\*.\***: Fitxers auxiliars del projecte.
- **data/Fifa2017_original.csv**: Dataset d'entrada (còpia del dataset original *FullData.csv*).
- **data/Fifa2017_final.csv**: Dataset de sortida, després del preprocessat.
- **pdf/Fifa2017-GK-vs-Others.pdf**: Fitxer de presentació amb les respostes de la pràctica.
- **src/Fifa2017-GK-vs-Others.Rmd**: Fitxer font amb les respostes de la pràctica que inclou el codi R.
- **src/Fifa2017-GK-vs-Others.R**: Extracció del codi font R emprat en la pràctica.
- **video/Fifa2017-GK-vs-Others.mp4**: Vídeo explicatiu del projecte.

## Autoria

Totes i cadascuna de les parts d'aquest treball han estat realitzades exclusivament de forma individual per **Nicolás González Soler**.

## Llicència

Shield: [![CC BY-NC-SA 4.0][cc-by-nc-sa-shield]][cc-by-nc-sa]

Tots i cadascun dels continguts d'aquest projecte estan sotmesos a la llicència
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa], excepte pel que respecta als datasets *original* i *processat* sobre els que caldria observar les llicències eventualment preexistents que són d'aplicació.

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg
