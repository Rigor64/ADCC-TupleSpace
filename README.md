# Erlang - Relazione progetto

## Tuple Space

<a name="readme-top"></a>

 Kania, Nicholas  - <n.kania@campus.uniurb.it> <br>
 Leopizzi, Matteo  - <m.leopizzi1@campus.uniurb.it> <br>
 Pierucci, Giada  - <g.pierucci4@campus.uniurb.it>

<!-- TABELLA DEI CONTENUTI -->
<details>
  <summary>Tabella dei contenuti</summary>
  <ol>
    <li>
      <a href="#descrizione-del-progetto">Descrizione del progetto</a>
    </li>
    <li>
      <a href="#tecnologia">Tecnologia</a>
      <ul>
        <li><a href="#dependances">Dependances</a></li>
        <li><a href="#scelte-implementative">Scelte implementative</a></li>
      </ul>
    </li>
    <li>
      <a href="#stress-test"> StressTest </a>
    </li>
  </ol>
</details>

<!-- DESCRIZIONE DEL PROGETTO -->
## Descrizione del progetto

Il progetto si propone di implementare uno **Spazio di Tuple** (Tuple Space - **TS**), ovvero un'astrazione di memoria condivisa, in cui i vari processi possono interagire mediante condivisione di messaggi (message-passing).

Dal momento in cui si verifica la **creazione** del TS in questione (`new(name)`) e vi si **aggiunge** almeno un nodo (`addNode(TS, Node)`), vi è la possibilità di eseguire due tipologie di operazioni di **lettura** ed una operazione di **scrittura** `out(TS, Tuple)`, la quale ha sempre esito positivo.

Le operazioni di lettura `in(TS, Pattern)` e `rd(TS, Pattern)` sono entrambe **bloccanti** sebbene solo la prima sia distruttiva, ovvero quando la tupla viene letta quest'ultima deve essere eliminata dal TS.
Si precisa che, data la natura bloccante delle operazioni in lettura, si definisce, per ognuna, un'ulteriore versione impostando un **timeout**.
Pertanto, qualora non venga riscontrata una tupla il cui pattern corrispondi a quello richiesto, si rimane in attesa solo fino a quando il periodo indicato non è decorso.

Oltre alle prescritte funzioni, vi è anche la possibilità di procedere alla **rimozione** un nodo precedentemente aggiunto nel TS qualora non fosse più d'interesse (`removeNode(TS, Node)`) e di osservare quali sono i nodi ancora presenti nel TS attraverso `nodes(TS)`.

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

* Modulo `tsm`: Tuple-Space Manager per gestire l'inizializzazione delle tabelle ETS e l'interfaccia del server.

* Modulo `ts`: nodi figli che ereditano la funzione `init()` del padre.

* Modulo `tstest`: batteria di Stress Test per qualificare le prestazioni e la resilienza del sistema.

* TrapExit: è stato implemenatto per tutelare il Server Tuple Space dalla caduta di un eventuale link non autorizzato

* ETS private, così da non esporre le tabelle ai nodi esterni

* Abbiamo implementato due tabelle ETS:

  * WhiteList (WL) : ETS per Pid autorizzati all'accesso. Tipologia set perchè contiene solo Pid e quest'ultimo è univoco, quindi lo utilizziamo come chiave
  * Space : ETS per la gestione dello spazio di tuple. Tipologia duplicate_bag per avere tuple duplicate e chiavi non univoche.

* WaitQueue : Lista temporanea per i messaggi in attesa (in , rd)

* add_node : non ha un controllo sugli accessi poichè se un nodo muore non potrebbe più linkarsi al tuple space a cui era apparteneva

* removeNode : viene rimosso il link tra nodo padre e figlio, questo ritorna un messaggio di EXIT a entrambi, quindi viene eliminato il nodo dalla WhiteList. Il nodo muori poichè ha ricevuto il messaggio di EXIT

<p align="right">(<a href="#readme-top">Torna su</a>)</p>

<!-- STRESS TEST -->
## Stress Test Result

<style>
#customers {
  font-family: Arial, Helvetica, sans-serif;
  border-collapse: collapse;
  width: 100%;
}
#customers td, #customers th {
  color: black;
  border: 1px solid #ddd;
  padding: 8px;
}
#customers tr:nth-child(even){background-color: #f2f2f2;}
#customers tr:hover {background-color: #ddd;}
#customers th {
  padding-top: 12px;
  padding-bottom: 12px;
  text-align: left;
  background-color: #04AA6D;
  color: white;
}
</style>

<table id = "customers">
<tr>
  <th>Company</th>
  <th>Contact</th>
  <th>Country</th>
</tr>
<tr>
  <td>Company</td>
  <td>Contact</td>
  <td>Country</td>
</tr>
</table>

1. Provare a rimuovere un Ts_actor e vedere se è ancora vivo.

2. Etteffuare una batteria di test per ogni operazione (in, rd, out).
