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

<!-- SCELTE IMPLEMENTATIVE -->
## Scelte implementative
___
<!-- MODULI -->
### MODULI

* Modulo `tss`: Tuple-Space Supervisor.
  Si occupa della supervisione del Tuple-Space Manager (`tsm`) e della sua inizializzazione. 
  Nel caso in cui il TS Manager decade lo rispristina. 


* Modulo `tsm`: Tuple-Space Manager. 
  Si occupa della creazione e gestione delle tabelle ETS: Tuple Space (TS), delle tabelle ETS (whitelist e l'interfaccia del server.

* Modulo `ts`: Tuple-Space Client.

* Modulo `tstest`: batteria di Stress Test per qualificare le prestazioni e la resilienza del sistema.
____
* MATCH SPECIFICATIONS: Il Pattern da seguire per poter inserire un record nel Tuple Space è il seguente:

  ```erl
    {'$1','$2', atomo, '$3'}
    % oppure
    {'_', '_', atomo, '_'}
  ```

* TrapExit: è stato implemenatto per tutelare il Server Tuple Space dalla caduta di un eventuale link non autorizzato



### DATASET

* Abbiamo implementato due tabelle ETS private, così da non esporre le tabelle ai nodi esterni:

  * **WhiteList (WL)** : registrazione dei Pid autorizzati all'accesso. Il Pid viene utilizzato come chiave data la natura del dato.
  * **Tuple Space (TS)** : gestore dello spazio di tuple. Tipologia duplicate_bag per avere tuple duplicate e chiavi non univoche.

* **WaitQueue** : Lista temporanea per i messaggi in attesa (in , rd)

### FUNZIONI

* `new(Name)` : crea un nuovo Tuple Space

* `in(TS, Pattern, Timeout)` : operazione di lettura dal Tuple Space, con eliminazione dell'elemento dalla tabella. Viene passato un valore di Timeout per scartare l'operazione in caso di mancata risposta. Se nel Tuple Space non è presente il Pattern specificato, la richiesta di lettura viene aggiunta alla Wait Queue.
* `in(TS, Pattern)` : funzione precedente, con `Timeout = infinity`
* `rd(TS, Pattern, Timeout)` : operazione di lettura dal Tuple Space. Viene passato un valore di Timeout per scartare l'operazione in caso di mancata risposta. Se nel Tuple Space non è presente il Pattern specificato, la richiesta di lettura viene aggiunta alla Wait Queue.
* `rd(TS, Pattern)` : funzione precedente, con `Timeout = infinity`
* `out(TS, Pattern)` : operazione di scrittura sul Tuple Space che, una volta eseguita, effettua un controllo sulla Wait Queue per verificare che ci siano richieste pendenti da poter soddisfare.

* `addNode(TS, Node)` : viene creato un link tra il processo invocante ed il gestore dello spazio di tuple, il quale registrerà il Pid dell'invocante all'interno della propria White List.
* `removeNode(TS, Node)` : viene eliminato il link con il gestore dello spazio di tuple, il nodo viene rimosso dalla White List e vengono eliminate tutte le richieste di `in` o `rd` relative a quel nodo.
* `nodes(TS, Node)` : elenco di tutti gli elementi contenuti nella White List.

<br />
<div align="center">
    <img src="data/TSM_process.png" alt="Screen1" width="900" height="500">
</div>
<br />



<br />
<div align="center">
    <img src="data/TSM_server_in_function.png" alt="Screen1" width="500" height="650">
</div>
<br />

<p align="right">(<a href="#readme-top">Torna su</a>)</p>

<!-- STRESS TEST -->
## Stress Test Result

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