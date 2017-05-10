 <?php
ob_start();
header("Content-type: text/javascript");

// Creamos la conexion con la base de datos
$con = @mysqli_connect('localhost', 'USUARIO_BD', 'PASSWORD_BD', 'NOMBRE_BD') or die("document.write('Error');");

// Obtenemos, y validamos enlace actual
$enlace = $_SERVER['HTTP_REFERER'];
if (!$enlace || $enlace == '') {
    die();
}

// Obtenemos los datos de la base de datos
$sql = "SELECT visitas FROM visitas WHERE enlace='$enlace'";
$query = mysqli_query($con, $sql);
$row = mysqli_fetch_assoc($query);

/*creamos los codigos querys verificando primero las cookies, para contar visitas y no impresiones web*/
if (isset($_COOKIE[md5($enlace)])) {
    // si existe la cookie solo le damos el valor a $visitas
    $visitas = $row['visitas'];
    echo "document.write($visitas);";
} elseif (!isset($_COOKIE[md5($enlace)])) {
    // Comprobamos si el enlace ya esta en la base de datos
    $rows = mysqli_num_rows($query);
    if ($rows > 0) {
        // Cuando exista lo enlace actualizamos
        $SQL = "UPDATE visitas SET visitas=visitas+1 WHERE enlace='$enlace'";
        if (mysqli_query($con, $SQL)) { // Si se inserta la visita
            $visitas = ($row['visitas']) + (1); // Le sumamos uno para mostrar la visita actual
            echo "document.write($visitas);";
            setcookie(md5($enlace), '_vStD', time() + 86400); // Y creamos la cookie de 1 dia
        } else { // Si no se inserta la visita
            $visitas = $row['visitas']; // Solo obtenemos las visitas
            echo "document.write($visitas);";
        }
    } elseif ($rows == 0) {
        // Cuando no existe el enlace en la base de datos la insertamos
        $SQL = "INSERT INTO visitas (enlace,visitas) VALUES ('$enlace',1)";
        if (mysqli_query($con,$SQL)) { // Si se inserta la nueva enlace
            echo "document.write(1);";
            setcookie(md5($enlace), '_vStD', time() + 86400); // Y creamos la cookie de 1 dia
        } else { // Si no se inserta mostramos
            echo "document.write(0);";
        }
    }
}

// Por ultimo cerramos la conexion, y cerramos el script
ob_end_flush();
mysqli_close($con);
die();
?>
