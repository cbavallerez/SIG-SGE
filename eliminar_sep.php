<?php
try {
    $mbd = new PDO('mysql:host=localhost;dbname=sigsge', sigsge, WA0k7A27GKp70GSm);
    foreach($mbd->query('SELECT * from ALUMNOS_SEP') as $fila) {
        print_r($fila);
    }
    $mbd = null;
} catch (PDOException $e) {
    print "¡Error!: " . $e->getMessage() . "<br/>";
    die();
}
?>
