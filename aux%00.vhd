-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2019-1 trabalho final, autor: Roberto Hexsel, 22/5
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- Acrescente modelos dos laboratorios a este arquivo


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- inversor
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity inv is
  generic (prop : time := t_inv);
  port(A : in bit;
       S : out bit);
end inv;

architecture comport of inv is 
begin
    S <= (not A);
end architecture comport;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta AND de 2 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity and2 is
  generic (prop : time := t_and2);
  port(A, B : in  bit;  -- entradas A,B
       S    : out bit); -- saida C
end and2;

architecture and2 of and2 is 
begin
    S <= A and B;
end and2;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta NAND de 2 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity nand2 is
  port(A,B : in bit;
       S   : out bit);
end nand2;

architecture comport of nand2 is 
begin
    S <= not(A and B);
end architecture comport;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta OR de 2 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity or2 is
  generic (prop : time := t_or2);
  port(A,B : in bit;
       S   : out bit);
end or2;

architecture comport of or2 is 
begin
  S <= reject t_rej inertial (A or B) after prop;
end architecture comport;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta OR de 3 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity or3 is
  generic (prop : time := t_or3);
  port(A, B, C : in  bit;  -- entradas A,B,C
       S       : out bit); -- saida S 
end or3;

architecture or3 of or3 is 
begin
    S <= A or B or C after prop;
end or3;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta XOR de 2 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity xor2 is
  port(A,B : in bit;
       S   : out bit);
end xor2;

architecture comport of xor2 is 
begin
  S <= reject t_rej inertial (A xor B) after t_xor2;
end architecture comport;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- porta XOR de 3 entradas
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity xor3 is
  generic (prop : time := t_xor3);
  port(A, B, C : in  bit;   -- entradas A,B,C
       S       : out bit);  -- saida S 
end xor3;

architecture xor3 of xor3 is 
begin
    S <= A xor B xor C after prop;
end xor3;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- N-bit register, synchronous load active in '0', asynch reset
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use work.p_WIRES.all;

entity registerN is
  generic (NUM_BITS: integer := 16;
           INIT_VAL: bit_vector);
  port(clk, rst, ld: in  bit;
       D:            in  bit_vector(NUM_BITS-1 downto 0);
       Q:            out bit_vector(NUM_BITS-1 downto 0));
end registerN;

architecture functional of registerN is
begin
  process(clk, rst, ld)
    variable state: bit_vector(NUM_BITS-1 downto 0);
  begin
    if rst = '0' then
      state := INIT_VAL;
    elsif rising_edge(clk) then
      if ld = '0' then
        state := D;
      end if;
    end if;
    Q <= state;
  end process;
  
end functional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- contador de 32 bits, reset=0 assincrono, load=1, enable=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.p_WIRES.all;

entity count32up is
  port(rel, rst, ld, en: in  bit;
        D:               in  reg32;
        Q:               out reg32);
end count32up;

architecture funcional of count32up is
  signal count: reg32;
begin

  process(rel, rst, ld)
    variable num : integer;
  begin
    if rst = '0' then
      count <= x"00000000";
    elsif ld = '1' then
      count <= D;
    elsif en = '1' and rising_edge(rel) then
      num := BV2INT(count) + 1;
      count <= INT2BV32(num);
    end if;
  end process;

  Q <= count after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- contador de 32 bits, reset=0 assincrono, load=1, enable=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all;
use work.p_WIRES.all;

entity count32dwn is
  port(rel, rst, ld, en: in  bit;
        D:               in  reg32;
        Q:               out reg32);
end count32dwn;

architecture funcional of count32dwn is
  signal count: reg32;
begin

  process(rel, rst, ld)
    variable num : integer;
  begin
    if rst = '0' then
      count <= x"00000000";
    elsif ld = '1' then
      count <= D;
    elsif en = '1' and rising_edge(rel) then
      num := BV2INT(count) - 1;
      count <= INT2BV32(num);
    end if;
  end process;

  Q <= count after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- registrador de 16 bits, reset=0 assincrono, load=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;

entity registrador16 is
  port(rel, rst, ld: in  bit;
        D:           in  reg16;
        Q:           out reg16);
end registrador16;

architecture funcional of registrador16 is
  signal value: reg16;
begin

  process(rel, rst, ld)
  begin
    if rst = '1' then
      value <= x"0000";
    elsif ld = '1' and rising_edge(rel) then
      value <= D;
    end if;
  end process;

  Q <= value after t_FFD;
end funcional;


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- registrador de 32 bits, reset=0 assincrono, load=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;

entity registrador32 is
  port(rel, rst, ld: in  bit;
        D:           in  reg32;
        Q:           out reg32);
end registrador32;

architecture funcional of registrador32 is
  signal value: reg32;
begin

  process(rel, rst, ld)
  begin
    if rst = '0' then
      value <= x"00000000";
    elsif ld = '1' and rising_edge(rel) then
      value <= D;
    end if;
  end process;

  Q <= value after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- registrador de 20 bits, reset=0 assincrono, load=1 sincrono
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;

entity registrador20 is
  port(rel, rst, ld: in  bit;
        D:           in  reg20;
        Q:           out reg20);
end registrador20;

architecture funcional of registrador20 is
  signal value: reg20;
begin

  process(rel, rst, ld)
  begin
    if rst = '0' then
      value <= (others => '0');
    elsif ld = '1' and rising_edge(rel) then
      value <= D;
    end if;
  end process;

  Q <= value after t_FFD;
end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- flip-flop tipo D com set,reset=0 assincronos
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;
entity FFD is
  port(rel, rst, set : in bit;
        D : in  bit;
        Q : out bit);
end FFD;

architecture funcional of FFD is
  signal estado : bit := '0';
begin

  process(rel, rst, set)
  begin
    if rst = '0' then
      estado <= '0';
    elsif set = '0' then
      estado <= '1';
    elsif rising_edge(rel) then
      estado <= D;
    end if;
  end process;

  Q <= estado after t_FFD;

end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- flip-flop tipo D com set,reset=0 assincronos, saidas Q e /Q
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_WIRES.all;
entity FFDQQ is
  port(rel, rst, set : in bit;
        D    : in  bit;
        Q, N : out bit);
end FFDQQ;

architecture funcional of FFDQQ is
  signal estado : bit := '0';
begin

  process(rel, rst, set)
  begin
    if rst = '0' then
      estado <= '0';
    elsif set = '0' then
      estado <= '1';
    elsif rising_edge(rel) then
      estado <= D;
    end if;
  end process;

  Q <= estado after t_FFD;
  N <= not estado after t_FFD;

end funcional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
						-- AQUI COMECA A EDICAO --
						    	-- SOMADORES --
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210                        autor: Roberto Hexsel, 19ago2013
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador completo de um bit, modelo estrutural
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity addBit is
  port(bitA, bitB, vem : in bit;    -- entradas A,B,vem-um
       soma, vai       : out bit);  -- saida C,vai-um
end addBit;

architecture estrutural of addBit is 
  component and2 is generic (prop:time);
                      port (A,B: in bit; S: out bit);
  end component and2;

  component or3 is generic (prop:time);
                      port (A,B,C: in bit; S: out bit);
  end component or3;

  component xor3 is generic (prop:time);
                      port (A,B,C: in bit; S: out bit);
  end component xor3;

  signal a1,a2,a3: bit;
begin
  U_xor:  xor3 generic map ( t_xor3 ) port map ( bitA, bitB, vem, soma );

  U_and1: and2 generic map ( t_and2 ) port map ( bitA, bitB, a1 );
  U_and2: and2 generic map ( t_and2 ) port map ( bitA, vem,  a2 );
  U_and3: and2 generic map ( t_and2 ) port map ( vem,  bitB, a3 );
  U_or:   or3  generic map ( t_or3  ) port map ( a1, a2, a3, vai );

end estrutural;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador de 16 bits, sem adiantamento de vai-um
-- Secao 1.6+8.1.2 de RH
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity adderCadeia is
  port(inpA, inpB : in reg16;
       outC : out reg16;
       vem  : in bit;
       vai  : out bit
       );
end adderCadeia;

architecture adderCadeia of adderCadeia is 
  component addBit port(bitA, bitB, vem : in bit;
                        soma, vai       : out bit);       
  end component addBit;

  signal v : reg16;                     -- cadeia de vai-um
  signal r : reg16;                     -- resultado parcial
begin

  -- entrada vem deve estar ligada em '0' para somar, em '1' para subtrair
  U_b0: addBit port map ( inpA(0), inpB(0), vem,  r(0), v(0) );
  U_b1: addBit port map ( inpA(1), inpB(1), v(0), r(1), v(1) );
  U_b2: addBit port map ( inpA(2), inpB(2), v(1), r(2), v(2) );
  U_b3: addBit port map ( inpA(3), inpB(3), v(2), r(3), v(3) );
  U_b4: addBit port map ( inpA(4), inpB(4), v(3), r(4), v(4) );
  U_b5: addBit port map ( inpA(5), inpB(5), v(4), r(5), v(5) );
  U_b6: addBit port map ( inpA(6), inpB(6), v(5), r(6), v(6) );
  U_b7: addBit port map ( inpA(7), inpB(7), v(6), r(7), v(7) );
  U_b8: addBit port map ( inpA(8), inpB(8), v(7), r(8), v(8) );
  U_b9: addBit port map ( inpA(9), inpB(9), v(8), r(9), v(9) );
  U_ba: addBit port map ( inpA(10),inpB(10),v(9), r(10),v(10) );
  U_bb: addBit port map ( inpA(11),inpB(11),v(10),r(11),v(11) );
  U_bc: addBit port map ( inpA(12),inpB(12),v(11),r(12),v(12) );
  U_bd: addBit port map ( inpA(13),inpB(13),v(12),r(13),v(13) );
  U_be: addBit port map ( inpA(14),inpB(14),v(13),r(14),v(14) );
  U_bf: addBit port map ( inpA(15),inpB(15),v(14),r(15),v(15) );
  
  vai <= v(15);
  outC <= r;
  
end adderCadeia;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- adiantamento de vai-um de 4 bits
--  P&H,2ndEd,sec4.5, RH sec1.6+8.3.2
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity adianta4 is
  port(a,b : in reg4;           -- entradas A(i),B(i)
       vem : in bit;            -- vem-um
       c: out reg4              -- vai(i)
       );
end adianta4;

architecture adianta4 of adianta4 is 
  component and2 is generic (prop:time);
                      port (A,B: in bit; S: out bit);
  end component and2;
  component or2 is generic (prop:time);
                      port (A,B: in bit; S: out bit);
  end component or2;

  signal p,g : reg4;
begin

  U_a0: and2 generic map ( t_and2 ) port map ( a(0), b(0), g(0) );
  U_a1: and2 generic map ( t_and2 ) port map ( a(1), b(1), g(1) );
  U_a2: and2 generic map ( t_and2 ) port map ( a(2), b(2), g(2) );
  U_a3: and2 generic map ( t_and2 ) port map ( a(3), b(3), g(3) );  

  U_o0: or2 generic map ( t_or2 ) port map ( a(0), b(0), p(0) );
  U_o1: or2 generic map ( t_or2 ) port map ( a(1), b(1), p(1) );
  U_o2: or2 generic map ( t_or2 ) port map ( a(2), b(2), p(2) );
  U_o3: or2 generic map ( t_or2 ) port map ( a(3), b(3), p(3) );

  c(0) <= g(0) or (p(0) and vem) after t_and2+t_or2;
  c(1) <= g(1) or (p(1) and g(0)) or (p(1) and p(0) and vem)
          after t_and3+t_or3;
  c(2) <= g(2) or (p(2) and g(1)) or (p(2) and p(1) and g(0)) or
          (p(2) and p(1) and p(0) and vem) after t_and4+t_or4;
  c(3) <= g(3) or (p(3) and g(2)) or (p(3) and p(2) and g(1)) or
          (p(3) and p(2) and p(1) and g(0)) or
          (p(3) and p(2) and p(1) and p(0) and vem)
          after t_and5+t_or5;

end adianta4;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador de 16 bits, com adiantamento de vai-um, 4 a 4 bits
--  P&H,2ndEd,sec4.5, RH sec8.3.2
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity adderAdianta4 is
  port(inpA, inpB : in reg16;
       outC : out reg16;
       vem  : in bit;
       vai  : out bit
       );
end adderAdianta4;

architecture adderAdianta4 of adderAdianta4 is 
  component addBit port(bitA, bitB, vem : in bit;
                        soma, vai       : out bit);       
  end component addBit;

  component adianta4 port(a,b : in reg4;
                          vem : in bit;
                          c: out reg4);
  end component adianta4;
  
  signal v : reg16;                     -- cadeia de vai-um
  signal r : reg16;                     -- resultado parcial
  signal c : reg16;                     -- aqcadeia de adiantamento de vai-um
begin

  -- entrada vem deve estar ligada em '0' para somar, em '1' para subtrair

  U_a0_3: adianta4 port map
    (inpA(3 downto 0),inpB(3 downto 0),vem,c(3 downto 0)); 

  U_b0: addBit port map ( inpA(0),inpB(0),vem, r(0),v(0) );
  U_b1: addBit port map ( inpA(1),inpB(1),c(0),r(1),v(1) );
  U_b2: addBit port map ( inpA(2),inpB(2),c(1),r(2),v(2) );
  U_b3: addBit port map ( inpA(3),inpB(3),c(2),r(3),v(3) );

  U_a4_7: adianta4 port map
    (inpA(7 downto 4),inpB(7 downto 4),c(3),c(7 downto 4));

  U_b4: addBit port map ( inpA(4),inpB(4),c(3),r(4),v(4) );
  U_b5: addBit port map ( inpA(5),inpB(5),c(4),r(5),v(5) );
  U_b6: addBit port map ( inpA(6),inpB(6),c(5),r(6),v(6) );
  U_b7: addBit port map ( inpA(7),inpB(7),c(6),r(7),v(7) );

  U_a8_b: adianta4 port map
    (inpA(11 downto 8),inpB(11 downto 8),c(7),c(11 downto 8)); 

  U_b8: addBit port map ( inpA(8), inpB(8), c(7), r(8), v(8) );
  U_b9: addBit port map ( inpA(9), inpB(9), c(8), r(9), v(9) );
  U_ba: addBit port map ( inpA(10),inpB(10),c(9), r(10),v(10) );
  U_bb: addBit port map ( inpA(11),inpB(11),c(10),r(11),v(11) );

  U_a12_15: adianta4 port map
    (inpA(15 downto 12),inpB(15 downto 12),c(11),c(15 downto 12)); 

  U_bc: addBit port map ( inpA(12),inpB(12),c(11),r(12),v(12) );
  U_bd: addBit port map ( inpA(13),inpB(13),c(12),r(13),v(13) );
  U_be: addBit port map ( inpA(14),inpB(14),c(13),r(14),v(14) );
  U_bf: addBit port map ( inpA(15),inpB(15),c(14),r(15),v(15) );
  
  vai <= c(15);
  outC <= r;
  
end adderAdianta4;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- adiantamento de vai-um de 16 bits
--  P&H,2ndEd,sec4.5, RH sec1.6+8.3.2
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity adianta16 is
  port(a,b : in reg16;          -- entradas A(i),B(i)
       vem : in bit;            -- vem-um
       c: out reg4              -- vai(i), de 4 em 4 bits
       );
end adianta16;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador de 16 bits, com adiantamento de vai-um de 16 bits
--  P&H,2ndEd,sec4.5, RH sec1.6+8.3.2
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- somador de 32 bits, com selecao de vai-um (carry select adder)
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use IEEE.numeric_std.all;
use work.p_wires.all;

entity adderCSA32 is
  port(inpA, inpB : in bit_vector;
       outC : out bit_vector;
       vem	: in bit;
       vai  : out bit);
  -- ---------------------------------------------------------
  function BV2INT(S: reg32) return integer is
    variable result: integer := 0; 
  begin
   if S(31) = '1' then result := -(1024*1024*2); else result := 0; end if;
    for i in S'range loop
      result := result * 2;
      if S(i) = '1' then
        result := result + 1;
      end if;
    end loop;
    return result;
  end BV2INT;
  -- ---------------------------------------------------------
  function SLV2BV32(s: std_logic_vector(31 downto 0)) return reg32 is
    variable result: reg32;
  begin
    for i in s'range loop
      if s(i) = '1' then
        result(i) := '1';
      else
        result(i) := '0';
      end if;
    end loop;
    return result;
  end SLV2BV32;
  -- ---------------------------------------------------------
  function INT2BV32(S: integer) return reg32 is
    variable result: reg32;
  begin
    result := SLV2BV32( std_logic_vector(to_signed(S,32)) );
    return result;
  end INT2BV32;
  -- ---------------------------------------------------------
 
end adderCSA32;


-- ======================================================================
-- especificação funcional para um somador de 32 bits; não computa vai-um
-- ======================================================================
architecture functional of adderCSA32 is 
  signal aint,bint,cint : integer;
begin

  aint <= BV2INT(inpA);
  bint <= BV2INT(inpB);  

  cint <= aint + bint;

  outC <= INT2BV32(cint);
  
  vai <= '0';
  
end architecture functional;

-- ======================================================================
-- escreva aqui seu modelo para o somador de 32 bits que computa vai-um
-- ======================================================================
 architecture structural of adderCSA32 is
-- 
   component adderAdianta16 is port(inpA, inpB : in bit_vector;
                           outC : out bit_vector;
                           vem  : in bit;
                           vai  : out bit);
   end component adderAdianta16;
 
    signal vem0, x,y,z : bit;
    signal s1,s2: reg16;
 begin
	
	-- Menos significativo.
	tsoma16: adderAdianta16 port map (inpA(15 downto 0),inpB(15 downto 0),outC(15 downto 0), '0', z);

	-- Mais significativo.
	psoma16: adderAdianta16 port map (inpA(31 downto 16),inpB(31 downto 16),s1, '0', x);
	ssoma16: adderAdianta16 port map (inpA(31 downto 16),inpB(31 downto 16),s2, '1', y);

	--mux tempo=55ps
	outC(31) <= s1(15) when z = '0' else s2(15) after 55 ps; -- joga psoma16 caso vai-um de 0, se não, joga ssoma16.
	outC(30) <= s1(14) when z = '0' else s2(14) after 55 ps;	
	outC(29) <= s1(13) when z = '0' else s2(13) after 55 ps;	
	outC(28) <= s1(12) when z = '0' else s2(12) after 55 ps;	
	outC(27) <= s1(11) when z = '0' else s2(11) after 55 ps;	
	outC(26) <= s1(10) when z = '0' else s2(10) after 55 ps;	
	outC(25) <= s1(9) when z = '0' else s2(9) after 55 ps;	
	outC(24) <= s1(8) when z = '0' else s2(8) after 55 ps;	
	outC(23) <= s1(7) when z = '0' else s2(7) after 55 ps;	
	outC(22) <= s1(6) when z = '0' else s2(6) after 55 ps;	
	outC(21) <= s1(5) when z = '0' else s2(5) after 55 ps;	
	outC(20) <= s1(4) when z = '0' else s2(4) after 55 ps;	
	outC(19) <= s1(3) when z = '0' else s2(3) after 55 ps;	
	outC(18) <= s1(2) when z = '0' else s2(2) after 55 ps;	
	outC(17) <= s1(1) when z = '0' else s2(1) after 55 ps;	
	outC(16) <= s1(0) when z = '0' else s2(0) after 55 ps;		
	
	vai <= (x OR y);

 end architecture structural;

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
						-- AQUI COMECA A EDICAO --
						    -- MULTIPLICADOR --
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 -- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- multiplica por 1: A(15..0)*B(i) => S(16..0)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use work.p_wires.all;

entity m_p_1 is
  port(A,B : in  reg16;                 -- entradas A,B
       S : in bit;                      -- bit por multiplicar
       R : out reg17);                  -- produto parcial
end m_p_1;

architecture funcional of m_p_1 is 

  component adderAdianta16 is port(inpA, inpB : in bit_vector;
                          outC : out bit_vector;
                          vem  : in  bit;
                          vai  : out bit);
  end component adderAdianta16;

  signal somaAB : reg17;

begin

  U_soma: adderAdianta16
    port map(A, B , somaAB(15 downto 0), '0', somaAB(16)); 

  -- defina a constante t_mux2 em packageWires.vhd
  R <= somaAB when S = '1' else ('0' & B);

end funcional;
-- -------------------------------------------------------------------


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- multiplicador combinacional
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use IEEE.std_logic_1164.all; use IEEE.numeric_std.all;
use work.p_wires.all;

entity mult16x16 is
  port(A, B : in  reg16;   -- entradas A,B
       prod : out reg32);  -- produto
end mult16x16;

-- ======================================================================
-- especificação funcional para um multiplicador de 32 bits
-- ======================================================================
architecture funcional of mult16x16 is 
begin
  prod <= INT2BV32( BV2INT16(A) * BV2INT16(B) );
end funcional;
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- ------------------------------------------------------------------
-- descomente as linhas com --x para acrescentar o código do multiplicador
-- ------------------------------------------------------------------

 architecture estrutural of mult16x16 is 
 
   component m_p_1 is port(A,B : in  bit_vector;   -- reg16
                           S   : in  bit;
                           R   : out bit_vector);  -- reg17
   end component m_p_1;
 
   signal p01,p02,p03,p04,p05,p06,p07,p08: reg17;
   signal p09,p10,p11,p12,p13,p14,p15,p16: reg17;
 
 begin
 
    U_00: m_p_1 port map (A ,x"0000" ,B(0) ,p01 );
    U_01: m_p_1 port map (A,p01(16 downto 1) ,B(1), p02);
    U_02: m_p_1 port map (A,p02(16 downto 1) ,B(2), p03);
    U_03: m_p_1 port map (A,p03(16 downto 1) ,B(3), p04);
    U_04: m_p_1 port map (A,p04(16 downto 1) ,B(4), p05);
    U_05: m_p_1 port map (A,p05(16 downto 1) ,B(5), p06);
    U_06: m_p_1 port map (A,p06(16 downto 1) ,B(6), p07);
    U_07: m_p_1 port map (A,p07(16 downto 1) ,B(7), p08);
    U_08: m_p_1 port map (A,p08(16 downto 1) ,B(8), p09);
    U_09: m_p_1 port map (A,p09(16 downto 1) ,B(9), p10);
    U_10: m_p_1 port map (A,p10(16 downto 1) ,B(10), p11);
    U_11: m_p_1 port map (A,p11(16 downto 1) ,B(11), p12);
    U_12: m_p_1 port map (A,p12(16 downto 1) ,B(12), p13);
    U_13: m_p_1 port map (A,p13(16 downto 1) ,B(13), p14);
    U_14: m_p_1 port map (A,p14(16 downto 1) ,B(14), p15);
    U_15: m_p_1 port map (A,p15(16 downto 1) ,B(15), p16);
    
 
    prod <=   p16 & p15(0) & p14(0) & p13(0) & p12(0) & p11(0) & p10(0) & p09(0) & p08(0) & p07(0) & p06(0) & p05(0) & p04(0) & p03(0) & p02(0) & p01(0);
   
 end estrutural;
 
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
						-- AQUI COMECA A EDICAO --
						        -- MUX --
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux2(a,b,s,z)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity mux2 is
  port(A,B : in  bit;                   -- entradas de dados
       S   : in  bit;                   -- entrada de selecao
       Z   : out bit);                  -- saida
end mux2;

architecture estrut of mux2 is 

  -- declara componentes que sao instanciados
  component inv is
  generic (prop : time := t_inv);
  port(A : in bit;
       S : out bit);
  end component inv;

  component nand2 is
    port(A,B : in bit; S : out bit);
  end component nand2;

  component nor2 is
    generic (prop : time; cont : time);
    port(A,B : in bit; S : out bit);
  end component nor2;

  component nand3 is
    generic (prop : time; cont : time);
    port(A,B,C : in bit; S : out bit);
  end component nand3;


  signal p, q, r: bit;  -- sinais internos
  
begin  -- compare ligacoes dos sinais com diagrama das portas logicas

  Ui:  inv   port map(s, r);
  Ua0: nand2 port map(a, r, p);
  Ua1: nand2 port map(b, s, q);
  Uor: nand2 port map(p, q, z);  
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux4(a,b,c,d,s0,s1,z)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity mux4 is
  port(A,B,C,D : in  bit;               -- quatro entradas de dados
       S0,S1   : in  bit;               -- dois sinais de selecao
       Z       : out bit);              -- saida
end mux4;

architecture estrut of mux4 is 

  component mux2 is
    port(A,B : in  bit; S : in  bit; Z : out bit);
  end component mux2;

  signal p,q,r : bit;                   -- sinais internos
begin

  -- copie seu o modelo da aula passada
    pmux: mux2 port map (a,b,s0,p);
    smux: mux2 port map (c,d,s0,q);
    tmux: mux2 port map (p,q,s1,z);

end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux8(a,b,c,d,e,f,g,h,s0,s1,s2,z)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity mux8 is
  port(A,B,C,D,E,F,G,H : in  bit;       -- oito entradas de dados
       S0,S1,S2        : in  bit;       -- tres sinais de controle
       Z               : out bit);      -- saida
end mux8;

architecture estrut of mux8 is 

  component mux2 is
    port(A,B : in  bit; S : in  bit; Z : out bit);
  end component mux2;

  component mux4 is
    port(A,B,C,D : in  bit; S0,S1 : in  bit; Z : out bit);
  end component mux4;

  signal p,q,r : bit;                   -- sinais internos
  
begin
  
  -- copie seu o modelo da aula passada
  pmux: mux4 port map (a,b,c,d,s0,s1,p);
  smux: mux4 port map (e,f,g,h,s0,s1,q);
  tmux: mux2 port map (p,q,s2,z);

end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- escreva a arquitetura when-else do mux8 aqui
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- escreva a arquitetura with-select do mux8 aqui
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- mux8vet(entr(7downto0),sel(2downto1),z)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity mux8vet is
  port(entr : in  reg8;
       sel  : in  reg3;
       Z    : out bit);
end mux8vet;

architecture estrut of mux8vet is 

  component mux2 is
    port(A,B : in  bit; S : in  bit; Z : out bit);
  end component mux2;

  component mux4 is
    port(A,B,C,D : in  bit; S0,S1 : in  bit; Z : out bit);
  end component mux4;

  signal p,q,r : bit;                   -- sinais internos
  
begin

  -- copie seu o modelo da aula passada
	pmux: mux4 port map (entr(0),entr(1),entr(2),entr(3),sel(0),sel(1),p);
	smux: mux4 port map (entr(4),entr(5),entr(6),entr(7),sel(0),sel(1),q);
	tmux: mux2 port map (p,q,sel(2),z);	
end architecture estrut;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- mux2_32b Foi é nada mais nada menos do que um mux2 "bit a bit, 32 vezes"

use work.p_wires.all;

entity mux2_32b is
  port(A, B	: in reg32;
       sel	: in bit;
       Z	: out reg32);
  end mux2_32b;

architecture estrut of mux2_32b is

	component mux2 is
		port (A, B	: in bit;
			  S		: in bit;
			  Z		: out bit);
	end component mux2;
	
begin
	
	m0:  mux2 port map(A(0),  B(0),  sel, Z(0));
    m1:  mux2 port map(A(1),  B(1),  sel, Z(1));
    m2:  mux2 port map(A(2),  B(2),  sel, Z(2));
    m3:  mux2 port map(A(3),  B(3),  sel, Z(3));
    m4:  mux2 port map(A(4),  B(4),  sel, Z(4));
    m5:  mux2 port map(A(5),  B(5),  sel, Z(5));
    m6:  mux2 port map(A(6),  B(6),  sel, Z(6));
    m7:  mux2 port map(A(7),  B(7),  sel, Z(7));
    m8:  mux2 port map(A(8),  B(8),  sel, Z(8));
    m9:  mux2 port map(A(9),  B(9),  sel, Z(9));
    m10: mux2 port map(A(10), B(10), sel, Z(10));
    m11: mux2 port map(A(11), B(11), sel, Z(11));
    m12: mux2 port map(A(12), B(12), sel, Z(12));
    m13: mux2 port map(A(13), B(13), sel, Z(13));
    m14: mux2 port map(A(14), B(14), sel, Z(14));
    m15: mux2 port map(A(15), B(15), sel, Z(15));
    m16: mux2 port map(A(16), B(16), sel, Z(16));
    m17: mux2 port map(A(17), B(17), sel, Z(17));
    m18: mux2 port map(A(18), B(18), sel, Z(18));
    m19: mux2 port map(A(19), B(19), sel, Z(19));
    m20: mux2 port map(A(20), B(20), sel, Z(20));
    m21: mux2 port map(A(21), B(21), sel, Z(21));
    m22: mux2 port map(A(22), B(22), sel, Z(22));
    m23: mux2 port map(A(23), B(23), sel, Z(23));
    m24: mux2 port map(A(24), B(24), sel, Z(24));
    m25: mux2 port map(A(25), B(25), sel, Z(25));
    m26: mux2 port map(A(26), B(26), sel, Z(26));
    m27: mux2 port map(A(27), B(27), sel, Z(27));
    m28: mux2 port map(A(28), B(28), sel, Z(28));
    m29: mux2 port map(A(29), B(29), sel, Z(29));
    m30: mux2 port map(A(30), B(30), sel, Z(30));
    m31: mux2 port map(A(31), B(31), sel, Z(31));

end estrut;

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--ESTE MUX TEM UMA ENTRADA SETADA EM 0 POIS ELA NAO E USADA
use work.p_wires.all;

entity mux32 is
  port(a	: in reg16;
       b,c	: in  reg32;
       sel  : in reg2;
       z    : out reg32);
end mux32;

architecture estrut of mux32 is 

  component mux2 is
    port(A,B : in  bit; 
		 S	 : in  bit; 
		 Z	 : out bit);
  end component mux2;

  signal p,q : bit;                     -- sinais internos
begin
    A1: process(a, b, c, sel)
    begin
      case sel is 
        when "00" => z <= x"0000"& a;
        when "01" => z <= b;
        when "10" => z <= c;
        when "11" => z <= x"00000000";
      end case;
    end process A1;
end architecture estrut;
