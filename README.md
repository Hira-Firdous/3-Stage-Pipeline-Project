# 3-Stage-Pipeline

## Description
A 3-stage pipeline generally refers to a computing or processing architecture that divides the execution of instructions into three main stages. This approach is common in the design of processors and digital systems to improve performance and efficiency
The above code contains the implementation of 3-Stage-Pipeline in system-verilog for risc-v
## Main modules
- PC
- Instruction memory
- Instruction decode
- Register file
- Immediate Generator
- Controller
- ALU
- Branch Comparator
- Hazard Unit
- CSR
- Data memory
- Muxes

## Installation
To execute the program you First must ensure that you have vsim properly installed on vscode or any IDE/terminal
### Command:
After installation of proper environment. Followings are the commands
- For comilation all the files:
  vlog *.sv
- For executing all the files:
  vsim -c tb_processor -voptargs=+acc=voptargs -do "run -all"
-For debugging and generating the:
  gtkwave processor.vcd
  

