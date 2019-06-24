-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- UFPR, BCC, ci210 2019-1 trabalho final, autor: Roberto Hexsel, 22/5
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- processador MICO XII
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;


entity mico is
  port (rst,clk : in bit);
end mico;

architecture functional of mico is
-- ==================================================================
-- ==================================================================
-- Componentes
-- ==================================================================
-- ==================================================================

-- Início: Componentes que já vieram
  component mem_prog is                 -- no arquivo mem.vhd
    port (ender : in  reg7;
          instr : out reg32);
  end component mem_prog;

  component display is                  -- neste arquivo
    port (rst,clk : in bit;
          enable  : in bit;
          data    : in reg32);
  end component display;

  component ULA is                      -- neste arquivo
    port (fun : in reg4;
          alfa,beta : in  reg32;
          gama      : out reg32);
  end component ULA;
 
  component R is                        -- neste arquivo
    port (clk         : in  bit;
          wr_en       : in  bit;
          r_a,r_b,r_c : in  reg4;
          A,B         : out reg32;
          C           : in  reg32);
  end component R;

  component RAM is                      -- neste arquivo
    port (rst, clk : in  bit;
          sel      : in  bit;           -- ativo em 1
          wr       : in  bit;           -- ativo em 1
          ender    : in  reg16;
          data_inp : in  reg32;
          data_out : out reg32);
  end component RAM;
  
-- Fim: Componentes que já vieram

-- Início: Componentes a mais
-- Mux2 32b (Check) -- muxB
-- Somador 16b (Check) -- ULA
-- Reg 16b/32b (Check16) (o de 16 bits nn funcionou)
-- Mult (check) -- ULA
-- Mux32 (Check) -- muxC

  component mux2_32b is
   port(A, B	: in reg32;
			sel	: in bit;
			Z	: out reg32);
   end component mux2_32b;

  component adderAdianta4 is
   port(inpA, inpB : in reg16;
       outC : out reg16;
       vem  : in bit;
       vai  : out bit
       );
   end component adderAdianta4;
   
  component mult16x16 is
	port(A, B	: in reg16;   
		 prod	: out reg32);
	end component mult16x16;
	
  component registrador16 is
	port(rel, rst, ld: in  bit;
        D:           in  reg16;
        Q:           out reg16);
	 end component registrador16;
	 
  component mux32 is
    port(a: in reg16;
         b,c  : in  reg32;
         sel  : in reg2;
         z    : out reg32);
    end component mux32;
	 
-- Fim: Componentes a mais

-- ==================================================================
-- ==================================================================
-- Tabela de controle
-- ==================================================================
-- ==================================================================  
  type t_control_type is record
    selNxtIP   : reg2;     -- seleciona fonte do incremento do IP
    selC       : reg2;     -- seleciona fonte da escrita no reg destino
    wr_reg     : bit;      -- atualiza banco de registradores
    selBeta    : bit;      -- seleciona fonte para entrada B da ULA
    mem_sel    : bit;      -- habilita acesso a RAM
    mem_wr     : bit;      -- habilita escrita na RAM
    wr_display : bit;      -- atualiza display=1
  end record;

  type t_control_mem is array (0 to 15) of t_control_type;
  
  constant ctrl_table : t_control_mem := (
  --sNxtIP selC  wrR selB  Msel Mwr wrDsp
    ("00", "00", '0', '0', '0', '0', '0'),            -- 0 NOP
    ("00", "01", '1', '0', '0', '0', '0'),            -- 1 ADD
    ("00", "01", '1', '0', '0', '0', '0'),            -- 2 SUB
    ("00", "01", '1', '0', '0', '0', '0'),            -- 3 MUL
    ("00", "01", '1', '0', '0', '0', '0'),            -- 4 AND
    ("00", "01", '1', '0', '0', '0', '0'),            -- 5 OR
    ("00", "01", '1', '0', '0', '0', '0'),            -- 6 XOR
    ("00", "01", '1', '0', '0', '0', '0'),            -- 7 NOT
    ("00", "01", '1', '1', '0', '0', '0'),            -- 8 ORI
    ("00", "01", '1', '1', '0', '0', '0'),            -- 9 ADDI
    ("00", "10", '1', '1', '1', '0', '0'),            -- A LD
    ("00", "10", '0', '1', '1', '1', '0'),            -- B ST
    ("00", "00", '0', '0', '0', '0', '1'),            -- C SHOW
    ("01", "00", '0', '0', '0', '0', '0'),            -- D JUMP
    ("11", "00", '0', '0', '0', '0', '0'),            -- E BRANCH           
    ("00", "00", '0', '0', '0', '0', '0'));           -- F HALT
    
  constant HALT : bit_vector := x"f";

  signal selNxtIP, selC : reg2;
  signal selBeta, wr_display, wr_reg, x, y: bit;
  signal mem_sel, mem_wr : bit;

  signal instr, A, B, C, beta, extended, ula_D, mem_D : reg32;
  signal this  : t_control_type;
  signal const, ip, ext_zeros, ext_sinal, prox_ip : reg16;
  signal opcode, reg_a, reg_b, reg_c : reg4;
  signal funct 	: reg6;
  signal i_opcode : natural range 0 to 15;
  
  signal ip_incrementado, destino, ip_beq : reg16;
  
begin  -- functional
-- ==================================================================
-- ==================================================================
-- Labels
-- Os Labels seguem a "ordem de fluxo" do Mips
-- MI -> R -> ULA -> RAM -> Finalização (R/IP/Display)
-- ==================================================================
-- ==================================================================  

-- Aqui está meu IP
  U_Ip: registrador16 port map(clk, rst, '1', prox_Ip, ip);
  
-- memoria de programa contem somente 128 palavras
  U_mem_prog: mem_prog port map(ip(6 downto 0), instr);

  opcode	<= instr(31 downto 28); -- Operação atual
  reg_a		<= instr(27 downto 24); -- endereço Registrador A
  reg_b		<= instr(23 downto 20); -- endereço Registrador B
  reg_c		<= instr(19 downto 16); -- endereço Registrador C
  const		<= instr(15 downto 0);  -- Em caso de Instrução ser do tipo I/J
  funct		<= instr(5 downto 0);   -- Em caso de nop

  i_opcode  <= BV2INT4(opcode);     -- indice do vetor DEVE ser inteiro

  this <= ctrl_table(i_opcode);         -- sinais de controle

  selBeta    <= this.selBeta;
  wr_display <= this.wr_display;
  selNxtIP   <= this.selNxtIP;
  wr_reg     <= this.wr_reg;
  selC       <= this.selC;
  mem_sel    <= this.mem_sel;
  mem_wr     <= this.mem_wr;

-- Label que já veio, Banco de Registradores
  U_regs: R port map (clk, wr_reg, reg_a, reg_b, reg_c, A, B, C);
  
-- Existe um Mux2_32b que recebe "B" e "ext",
-- preciso primeiro saber se estou em OP = 0100, se sim, devo passar com sinal
  ext_zeros(15 downto 0) <= x"0000"; -- unsigned
  ext_sinal(15 downto 0) <= x"FFFF"; -- com sinal
  
  extended <= ext_sinal(15 downto 0) & const(15 downto 0) when opcode = "0010" or const(15) = '1' 
  else ext_zeros(15 downto 0) & const(15 downto 0); -- Passa sem sinal.
 
  muxB:	mux2_32b port map(B, extended, selBeta, beta);

-- Calcula prox. Instrução
  soma_ip_incremento: adderAdianta4 port map (ip, x"0001", ip_incrementado, '0', x);
  
-- Calcula desvio condicional de beq (OP = E (1110)) depois verifica se deve ou não pular
  verifica_beq:
  process(A, B, ip_beq, ip_incrementado) begin
    if (A = B) then
		destino <= const;
	else
		destino <= ip_incrementado;
	end if;
	end process;
	
-- Label que já veio, ULA		   
  U_ULA: ULA port map (opcode, A, beta, ula_D);

-- Label que já veio, Memória Ram
  U_mem: RAM port map (rst, clk, mem_sel, mem_wr, ula_D(15 downto 0), B, mem_D);

-- Responsável por dizer quem vai para C do Banco de Registradores.
  muxC: mux32 port map (ip_incrementado, ula_D, mem_D, selC, C); 
  
-- nao altere esta linha
  U_display: display port map (rst, clk, wr_display, A);

-- Mux prox.IP 
-- Vale lembrar que o comando "with" funciona como process
-- Aqui verifico, baseado no sinal de controle, se segue o jogo, pula, ou se estou em beq
  with selNxtIP select
	prox_ip <= ip_incrementado when "00", -- IP+1
			   const when "01", -- j fazendo pulo
			   A(15 downto 0) when "10", -- JR
			   destino when others; -- Beq
  
  assert opcode /= HALT
    report LF & LF & "simulation halted: " & 
    "ender = "&integer'image(BV2INT16(ip))&" = "&BV16HEX(ip)&LF
    severity failure;
  
end functional;
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ==================================================================
-- ==================================================================
-- ULA
-- ==================================================================
-- ==================================================================
use work.p_wires.all;

entity ULA is
  port (fun : in reg4;
        alfa,beta : in  reg32;
        gama      : out reg32);
end ULA;

architecture behaviour of ULA is

  component adderCSA32 is
   port(inpA, inpB : in bit_vector;
       outC : out bit_vector;
       vem	: in bit;
       vai  : out bit);
  end component adderCSA32;
  
  component mult16x16 is
    port(A, B : in  reg16;   -- entradas A,B
		 prod : out reg32);  -- produto
  end component mult16x16;
  
-- signal
  signal x, y	: bit;
  signal s0, s1, s2: reg32;

begin  -- behaviour

-- Operações que utilizam os blocos
soma:	adderCSA32 port map(alfa, beta, s0, '0', x); -- OP = 0001
sub:	adderCSA32 port map(alfa, beta, s1, '1', y); -- OP = 0010
mult:	mult16x16  port map(alfa(15 downto 0), beta(15 downto 0), s2); -- OP = 0011

-- Procedimento responsável pela seleção do que sai em Gama
seletor_ula: process(fun, s0, s1, s2, alfa, beta)

-- Operações que utilizam lógica booleana
variable
	andAB, orAB, xorAB	: reg32;
	
begin	
	andAB	:= alfa and beta;
	orAB	:= alfa or beta;
	xorAB	:= alfa xor beta;
	
-- Mux com saida para Gama
	case fun is
	when "0000"	=> gama <= x"00000000"; -- 0 Nop
	when "0001"	=> gama <= s0;			-- 1 Soma
	when "0010"	=> gama <= s1;			-- 2 Sub
	when "0011"	=> gama <= x"0000" & s2 (15 downto 0);			-- 3 Mult
	when "0100"	=> gama <= andAB;		-- 4 And
	when "0101"	=> gama <= orAB;		-- 5 Or
	when "0110"	=> gama <= xorAB;		-- 6 Xor
	when "0111"	=> gama <= not alfa;	-- 7 Not
	when "1000"	=> gama <= orAB;		-- 8 Or (const)
	when "1001"	=> gama <= s0;			-- 9 Soma (const)
	when "1010"	=> gama <= s0;			-- A Ld
	when "1011"	=> gama <= s0; 			-- B St
	when "1100"	=> gama <= x"00000000"; -- C Show
	when "1101"	=> gama <= x"00000000"; -- D Jump
	when "1110"	=> gama <= x"00000000"; -- E Beq
	when "1111"	=> gama <= x"00000000"; -- F Halt
	
	end case;

end process seletor_ula;
 
end behaviour;
-- -----------------------------------------------------------------------



-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- display: exibe inteiro na saida padrao do simulador
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE; use std.textio.all;
use work.p_wires.all;

entity display is
  port (rst,clk : in bit;
        enable  : in bit;
        data    : in reg32);
end display;

architecture functional of display is
  file output : text open write_mode is "STD_OUTPUT";
begin  -- functional

  U_WRITE_OUT: process(clk)
    variable msg : line;
  begin
    if falling_edge(clk) and enable = '1' then
      write ( msg, string'(BV32HEX(data)) );
      writeline( output, msg );
    end if;
  end process U_WRITE_OUT;

end functional;
-- ++ display ++++++++++++++++++++++++++++++++++++++++++++++++++++++++



-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- memoria RAM, com capacidade de 64K palavras de 32 bits
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ieee; use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.p_wires.all;

entity RAM is
  port (rst, clk : in  bit;
        sel      : in  bit;          -- ativo em 1
        wr       : in  bit;          -- ativo em 1
        ender    : in  reg16;
        data_inp : in  reg32;
        data_out : out reg32);

  constant DATA_MEM_SZ : natural := 2**16;
  constant DATA_ADDRS_BITS : natural := log2_ceil(DATA_MEM_SZ);

end RAM;

architecture rtl of RAM is
  
  subtype t_address is unsigned((DATA_ADDRS_BITS - 1) downto 0);
  
  subtype word is bit_vector(31 downto 0);
  type storage_array is
    array (natural range 0 to (DATA_MEM_SZ - 1)) of word;
  signal storage : storage_array;
begin
  
  accessRAM: process(rst, clk, sel, wr, ender, data_inp)
    variable u_addr : t_address;
    variable index, latched : natural;

    variable d : reg32 := (others => '0');
    variable val, i : integer;

  begin

    if (rst = '0') and (sel = '1') then -- normal operation

      index := BV2INT16(ender);

      if  (wr = '1') and rising_edge(clk) then
        
        assert (index >= 0) and (index < DATA_MEM_SZ)
          report "ramWR index out of bounds: " & natural'image(index)
          severity failure;

        storage(index) <= data_inp;
        
        assert TRUE report "ramWR["& natural'image(index) &"] "
          & BV32HEX(data_inp); -- DEBUG
        
      else

        assert (index >= 0) and (index < DATA_MEM_SZ)
          report "ramRD index out of bounds: " & natural'image(index)
          severity failure;

        d := storage(index);
        
        assert TRUE report "ramRD["& natural'image(index) &"] "
          & BV32HEX(d);  -- DEBUG

      end if; -- normal operation

      data_out <= d;

    else

      data_out <= (others=>'0');

    end if; -- is reset?
    
  end process accessRAM; -- ---------------------------------------------
  
end rtl;
-- -----------------------------------------------------------------------



-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- banco de registradores
--          NAO ALTERE ESTE MODELO
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
use work.p_wires.all;

entity R is
  port (clk         : in  bit;
        wr_en       : in  bit;          -- ativo em 1
        r_a,r_b,r_c : in  reg4;
        A,B         : out reg32;
        C           : in  reg32);
end R;

architecture rtl of R is
  type reg_file is array(0 to 15) of reg32;
  signal reg_file_A : reg_file;
  signal reg_file_B : reg_file;
  signal int_ra, int_rb, int_rc : integer range 0 to 15;
begin

  int_ra <= BV2INT4(r_a);
  int_rb <= BV2INT4(r_b);
  int_rc <= BV2INT4(r_c);

  A <= reg_file_A( int_ra ) when r_a /= b"0000" else
       x"00000000";                        -- reg0 always zero
  B <= reg_file_B( int_rb ) when r_b /= b"0000" else
       x"00000000";

  WRITE_REG_BANKS: process(clk)
  begin
    if rising_edge(clk) then
      if wr_en = '1' and r_c /= b"0000" then
        reg_file_A( int_rc ) <= C;
        reg_file_B( int_rc ) <= C;
      end if;
    end if;
  end process WRITE_REG_BANKS;
  
end rtl;
-- -----------------------------------------------------------------------
