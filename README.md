# Erlang - Relazione progetto

## Tuple Space

<a name="readme-top"></a>

 Kania, Nicholas 	- n.kania@campus.uniurb.it <br>
 Leopizzi, Matteo 	- m.leopizzi1@campus.uniurb.it <br>
 Pierucci, Giada 	- g.pierucci4@campus.uniurb.it

<!-- TABELLA DEI CONTENUTI -->
<details>
  <summary>Tabella dei contenuti</summary>
  <ol>
    <li>
      <a href="#descrizione-del-progetto">Descrizione del progetto</a>
    </li>
    <li>
      <a href="#use-cases">Use cases</a>
    </li>
    <li>
      <a href="#user-experience">User experience</a>
    </li>
    <li>
      <a href="#tecnologia">Tecnologia</a>
      <ul>
        <li><a href="#dependances">Dependances</a></li>
        <li><a href="#scelte-implementative">Scelte implementative</a></li>
      </ul>
    </li>
  </ol>
</details>

<!-- DESCRIZIONE DEL PROGETTO -->
## Descrizione del progetto

Il progetto si propone di ...

<p align="right">(<a href="#readme-top">Torna su</a>)</p>

<!-- USE CASES -->
## Use cases


<p align="right">(<a href="#readme-top">Torna su</a>)</p>

<!-- TECNOLOGIA -->
## Tecnologia
<!-- DEPENDANCES -->
### Dependances



```erl
% codice
```

<p align="right">(<a href="#readme-top">Torna su</a>)</p>

<!-- SCELTE IMPLEMENTATIVE -->
### Scelte implementative

* Modulo ts_actor : inizializzazione delle tabelle ETS e interfaccia del server

* Modulo ts : 

* TrapExit: è stato implemenatto per tutelare il Server Tuple Space dalla caduta di un eventuale link non autorizzato
* ETS private, così da non esporre le tabelle ai nodi esterni
* removeNode : viene eliminato il nodo dalla WhiteList, il nodo non decade, ma non ha più la possibilità di accedere alle tabelle ETS
* Abbiamo implementato due tabelle ETS:

  * WhiteList (WL) : ETS per Pid autorizzati all'accesso. Tipologia set perchè contiene solo Pid e quest'ultimo è univoco, quindi lo utilizziamo come chiave
  * Space : ETS per la gestione dello spazio di tuple. Tipologia duplicate_bag per avere tuple duplicate e chiavi non univoche.

* add_node : non ha un controllo sugli accessi poichè se un nodo muore non potrebbe più linkarsi al tuple space a cui era apparteneva

* remove_node : ha un controllo per verificare che sia prendete il nodo nella tabella ETS

* WaitQueue : Lista temporanea per i messaggi in attesa (in , rd)

<p align="right">(<a href="#readme-top">Torna su</a>)</p>


Stress Test:

provare a rimuovere un Ts_actor e vedere se è ancora vivo