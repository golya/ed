<?php

$response = array(

    "requests" => array(
        array(
            "script_filename" => "/sum.php",
            "query_string" => "from=1&to=250000000"
        ),
        array(
            "script_filename" => "/sum.php",
            "query_string" => "from=250000001&to=500000000"
        ),
        array(
            "script_filename" => "/sum.php",
            "query_string" => "from=500000001&to=750000000"
        ),
        array(
            "script_filename" => "/sum.php",
            "query_string" => "from=750000001&to=1000000000"
        )
    )
);

echo json_encode($response);
?>