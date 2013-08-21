<?php
$start = microtime(true);
function sum($from, $to) {
    $result = 0;
    for($i = $from; $i <= $to; $i++) {
        $result += $i;
    }
    return $result;
}

$from = $_GET['from'];
$to = $_GET['to'];

sum($from, $to);

$end = microtime(true);
$diff = $end - $start;

$response = "$from -> $to: $start $end $diff";

echo json_encode($response);
?>