<?php
$row = 1;

$fp = fopen('acm.csv', 'w');

$outros = [];

$handle = fopen("dados.csv", "r");
while (($data = fgetcsv($handle, null, ",")) !== FALSE) {

    $line = [];

    if ($row == 1) {
        $line = [
            "Graduação",
            "Pós-Graduação",
            "UsoR",
            "UsoSoftEstatistico",
            "UsoSoftPlanilha",
            "UsoLingProgramacao",
            "UsoSoftEducacional",
        ];
    } else {

        $line[0] = '';

        if (empty($data[4]) or preg_match('/(Administra|Computa|Física|Pedagogia|Veterinária|Zootecnia|Contábeis|Contabeis|Química|Geografia|Sociais|Atmosféricas|Agronomia|Geologia|Processamento)/i', $data[4]))
            $line[0] = 'Outra';

        if (preg_match('/(Enfermagem|Biomedicina|Nutrição|Odontologia|Saúde|Fisioterapia|Psicologia)/i', $data[4]))
            $line[0] = 'Saúde';

        if (preg_match('/(Biologia|Bioló)/i', $data[4]))
            $line[0] = 'Biologia';

        if (preg_match('/(Economia|Econômicas|Atuariais)/i', $data[4]))
            $line[0] = 'Economia';

        if (preg_match('/(Engenharia)/i', $data[4]))
            $line[0] = 'Engenharia';

        if (preg_match('/(Estat)/i', $data[4]))
            $line[0] = 'Estatística';

        if (preg_match('/(Matem)/i', $data[4]))
            $line[0] = 'Matemática';

        if (empty($line[0])) {
            var_dump($data[4]);
            exit;
        }

        $line[] = "PosEmEstatistica_" . $data[5];

        $r = ['Rstudio', 'Linguagem R', 'R Commander'];
        $educ = ["geogebra"];
        $planilhas = ["Google sheet", "Google Planilha", "CALC", 'Office', 'Excel'];
        $estatisticos = ["SAS", "Stata", "Gretl", "Statistica", "Bioest", 'BIOSTAT', 'Past', 'Power Bi', 'JASP', 'Jamovi', 'Eviews', 'Systat', 'Sisvar'];
        $linguagens = ["C\+\+", "Fortran", "Java", "Python", "Matlab", 'Julia', 'Octave'];

        $line[] = "UsoR_" . traduzUso([$data[6], localiza($data, $r)]);
        $line[] = "UsoSoftEstatistico_" . traduzUso([$data[10], $data[12], $data[14], $data[15], localiza($data, $estatisticos)]);
        $line[] = "UsoSoftPlanilha_" . traduzUso([$data[9], localiza($data, $planilhas)]);
        $line[] = "UsoLingProgramacao_" . traduzUso([$data[13], localiza($data, $linguagens)]);
        $line[] = "UsoSoftEducacional_" . traduzUso([$data[7], $data[8], $data[11], localiza($data, $educ)]);

        $softwares = array_merge($r, $educ, $planilhas, $estatisticos, $linguagens);
        $reg = "/(" . implode("|",  $softwares) . ")/i";

        if (!empty($data[16]) and !preg_match($reg, $data[16]))
            @$outros[$data[16]]++;

        if (!empty($data[18]) and !preg_match($reg, $data[18]))
            @$outros[$data[18]]++;

        if (!empty($data[20]) and !preg_match($reg, $data[20]))
            @$outros[$data[20]]++;
    }

    fputcsv($fp, $line);

    $row++;
}

//arsort($outros);
//print_r($outros);

fclose($handle);
fclose($fp);

function clusters($v, $inicio, $incremento, $label)
{
    if (empty($v))
        return '';
    do {
        $fim = $inicio + $incremento;
        if ($v >= $inicio and $v < $fim)
            break;
        $inicio = $fim;
    } while (true);
    return $label . ' ' . sprintf("%02d-%02d", $inicio, $fim - 1);
}


function localiza($data, $itens)
{
    $reg = "/(" . implode("|", $itens) . ")/i";
    $ret = [0];
    if (preg_match($reg, $data[16]))
        $ret[] = $data[17];
    if (preg_match($reg, $data[18]))
        $ret[] = $data[19];
    if (preg_match($reg, $data[20]))
        $ret[] = $data[21];
    return max($ret);
}

function traduzUso($v)
{
    if (is_array($v))
        $v = max($v);

    if (empty($v) or $v < 3)
        return 'Não';

    return 'Sim';
}
