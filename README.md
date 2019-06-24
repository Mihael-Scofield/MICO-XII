# Mico XII, Implementado por Mihael Scofield de Azevedo e Thauan de Souza Tavares. 
# Feito originalmente por Roberto Hexsel.
# Aplicado como trabalho da disciplina ci210 (UFPR) por Eduardo Todt em 2019-1.

O Mico XII é um microprocessador de arquitetura MIPS, que executa 16 funções, todo implementado em VHDL,
a explicação de seus arquivos segue abaixo:

aux.vhd -- contém alguns dos modelos usados até agora; acrescente somente
           modelos usados nos laboratorios a este arquivo.

mem.vhd -- seu programa em assembly do Mico deverá ser escrito neste
	   arquivo, que já contém um programa para testar algumas instruções.

mico.vhd -- TODO o seu código novo deverá ser escrito neste arquivo, e
            somente neste arquivo.

packageWires.vhd -- contém as definições de tipos e funções necessários
                    para completar seu projeto.

tb_mico.vhd -- contém o gerador de relógio e de reset, e instancia o mico.

run.sh -- executa o simulador.  Os argumentos de linha de comando são
          diferentes daqueles usados nos laboratórios. Diga  ./run.sh -?

v.sav -- configuração do gtkwave.
