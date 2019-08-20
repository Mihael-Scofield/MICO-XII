# Mico XII
Implementado por Mihael Scofield de Azevedo e Thauan de Souza Tavares. 
Feito originalmente por Roberto Hexsel.
Aplicado como trabalho da disciplina Projetos Digitais e Microprocessadores - ci210 (UFPR) por Eduardo Todt em 2019-1.

## Resumo
O Mico XII é um microprocessador de arquitetura MIPS, que executa 16 funções, todo implementado em VHDL,
a explicação de seus arquivos segue abaixo:

## Estrutura
- aux.vhd -- contém alguns dos modelos usados para auxiliar no projeto.

- mem.vhd -- aqui está o programa em assembly executado pelo Mico.

- mico.vhd -- Todo o VHDL pesado está aqui, contendo a estrutura do Mico em si.

- packageWires.vhd -- contém as definições de tipos e funções necessárias.

- tb_mico.vhd -- contém o gerador de relógio e de reset, e instancia o mico.

- run.sh -- executa o simulador. Diga  ./run.sh -?

- v.sav -- configuração do gtkwave.
