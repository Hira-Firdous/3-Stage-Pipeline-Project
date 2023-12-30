module processor 
(
    input logic clk,
    input logic rst
); 
    // wires
    logic        rf_en_DE;
    logic        rf_en_DM;


    logic        sel_b;
    logic [31:0] pc_out_IF;
    logic [31:0] pc_out_DE;
    logic [31:0] new_pc;


    logic  [ 1:0] stall_IF;
    logic  [ 1:0] flush_DE;

    logic [31:0] csr_epc_IF;
    logic [31:0] csr_epc_WB;


    logic [31:0] csr_pc;


    logic [31:0] inst_IF;
    logic [31:0] inst_DE;


    logic [ 4:0] rd;
    logic [ 4:0] rs1;
    logic [ 4:0] rs2;
    logic [ 6:0] opcode;
    logic [ 2:0] funct3;
    logic [ 6:0] funct7;
    logic [31:0] rdata1;
    logic [31:0] rdata2;
    logic [31:0] opr_a;
    logic [31:0] opr_b;


    logic [31:0] opr_res_IF;
    logic [31:0] opr_res_DE;
    logic [31:0] opr_res_DM;


    logic [11:0] imm;
    logic [31:0] imm_val;


    logic [31:0] wdata_DE;
    logic [31:0] wdata_MW;

    logic [ 1:0] Forward_op_A;  
    logic [ 1:0] Forward_op_B;
    logic [31:0] opr_A_forward;
    logic [31:0] opr_B_forward;
    logic [3 :0] aluop;
    logic [31:0] rdata;

    //control signals
    logic        rd_en_DE;
    logic        rd_en_DM;
    logic        wr_en_DE;
    logic        wr_en_DM;
    logic [ 1:0] wb_sel_DE;
    logic [ 1:0] wb_sel_DM;
    logic        br_taken_IF;
    logic        br_taken_DE;


    

    logic [ 2:0] br_type;

    logic [ 2:0] mem_acc_mode_DE;
    logic [ 2:0] mem_acc_mode_DM;
    

    logic [31:0] rdata_csr;
    logic       csr_reg_wrMW_DE;  
    logic       csr_reg_wrMW_DM;  
    logic       csr_reg_rdMW_DE;
    logic       csr_reg_rdMW_DM;
    logic       is_mret_DE;
    logic       is_mret_DM;
    logic       epc_taken_MW;
    
    logic [31:0] epc;





//-------------------------------------Instruction fetch-----------------------------------
    // PC MUX
    mux_2x1 mux_2x1_pc
    (
        .in_0        ( pc_out_IF + 32'd4 ),
        .in_1        ( opr_res_IF        ),
        .select_line ( br_taken_IF       ),
        .out         ( new_pc            )
    );

    //csr PC
     mux_2x1 mux_2x1_pc2
    (
        .in_0        ( new_pc         ),
        .in_1        ( csr_epc_IF      ),
        .select_line ( epc_taken      ),
        .out         ( csr_pc         )
    );

    

    // program counter
    pc pc_i
    (
        .clk   ( clk            ),
        .rst   ( rst            ),
        .en    ( stall_IF       ),
        .pc_in ( csr_pc         ),
        .pc_out( pc_out_IF      )
    );

    // instruction memory
    inst_mem inst_mem_i
    (
        .addr  ( pc_out_IF      ),
        .data  ( inst_IF        )
    );


//---------------------------------------------------
always_ff @( posedge clk ) 
    begin  
        if(rst)
        begin
            inst_DE<=0;
            pc_out_DE<=0;
        end
        else if (~stall_IF)
        begin
            inst_DE<=inst_IF;
            pc_out_DE<=pc_out_IF;
        end
    end

    

//-----------------------------Decode and Execude------------------------------


always_ff @( posedge clk ) 
    begin  
        if(flush_DE)
        begin
            inst_DE <=32'h00000013;
            pc_out_DE='b0;
        end
    end


    // instruction decoder
    inst_dec inst_dec_i
    (
        .inst  ( inst_DE        ),
        .rs1   ( rs1            ),
        .rs2   ( rs2            ),
        .rd    ( rd             ),
        .opcode( opcode         ),
        .funct3( funct3         ),
        .funct7( funct7         )
    );

    // register file
    reg_file reg_file_i
    (
        .clk   ( clk            ),
        .rf_en ( rf_en_DE       ),
        .rd    ( rd             ),
        .rs1   ( rs1            ),
        .rs2   ( rs2            ),
        .rdata1( rdata1         ),
        .rdata2( rdata2         ),
        .wdata ( wdata_DE       )
    );



    // immediate generator
    imm_gen imm_gen_i
    (
        .inst   ( inst_DE       ),
        .imm_val( imm_val       )
    );


    always_comb
    begin
        case(Forward_op_A)
            2'b00:   opr_A_forward = rdata1;
            2'b10:   opr_A_forward = opr_res_DM;
            default: opr_A_forward = 'b0;
        endcase
    end


     // ALU opr_a MUX
    mux_2x1 mux_2x1_alu_opr_a
    (
        .in_0           ( pc_out_DE      ),
        .in_1           ( opr_A_forward  ),
        .select_line    ( sel_a          ),
        .out            ( opr_a          )
    );

    always_comb
    begin
        case(Forward_op_B)
            2'b00:   opr_B_forward = rdata2;
            2'b10:   opr_B_forward = opr_res_DM;
            default: opr_B_forward = 'b0;
        endcase
    end
    

    // ALU opr_b MUX
    mux_2x1 mux_2x1_alu_opr_b
    (
        .in_0           ( opr_B_forward  ),
        .in_1           ( imm_val ),
        .select_line    ( sel_b   ),
        .out            ( opr_b   )
    );


    br_cond br_cond
    (
        .br_type   ( br_type         ),
        .rdata1    ( opr_a           ),
        .rdata2    ( opr_b           ),
        .br_taken  ( br_taken        )
    );


    // alu
    alu alu_i
    (
        .aluop   ( aluop          ),
        .opr_a   ( opr_a          ),
        .opr_b   ( opr_b          ),
        .opr_res ( opr_res_DE     )
    );

       // controller
    controller controller_i
    (
        .inst           ( inst_DE           ),
        .opcode         ( opcode            ),
        .funct3         ( funct3            ),
        .funct7         ( funct7            ),
        .br_taken       ( br_taken_DE       ),
        .is_mret        ( is_mret_DE        ),
        .aluop          ( aluop             ),
        .rf_en          ( rf_en_DE          ),
        .sel_a          ( sel_a             ),
        .sel_b          ( sel_b             ),
        .csr_reg_wrMW   ( csr_reg_wrMW_DE   ),  
        .csr_reg_rdMW   ( csr_reg_rdMW_DE   ),    
        .rd_en          ( rd_en_DE          ),
        .wr_en          ( wr_en_DE          ),
        .wb_sel         ( wb_sel_DE         ),
        .mem_acc_mode   ( mem_acc_mode_DE   ),
        .br_type        ( br_type            )
    );



//------------------------------------------------
    //Feedback: 
    always_comb
     begin  
        br_taken_IF =br_taken_DE;
        opr_res_IF=opr_res_DM;
    end

 
// --------------------- Memory and Writeback ---------------------

always_ff @( posedge clk ) 
    begin  
        if(rst)
        begin
            opr_res_DM<=0;
            rd_en_DM<=0;
            wr_en_DM<=0;
            mem_acc_mode_DM<=0;
            wb_sel_DM<=0;
            csr_reg_wrMW_DM=0;
            csr_reg_rdMW_DM=0;
            is_mret_DM<=0;
        end
        else
        begin
            opr_res_DM<=opr_res_DE;
            rd_en_DM <= rd_en_DE;
            wr_en_DM<=wr_en_DE;
            mem_acc_mode_DM<=mem_acc_mode_DE;
            wb_sel_DM<=wb_sel_DE;
            csr_reg_wrMW_DM<=csr_reg_wrMW_DE;
            csr_reg_rdMW_DM<=csr_reg_rdMW_DE;
            is_mret_DM<=is_mret_DE;
        end
    end
    //Isntruction for CSR
    CRS csr
(
        .clk             (clk          ),
        .addr            (opr_res_DM   ),
        .data            (rdata2       ),
        .csr_reg_wrMW   ( csr_reg_wrMW_DM ),  
        .csr_reg_rdMW   ( csr_reg_rdMW_DM ), 
        .is_mret        ( is_mret_DM      ),
        .pc              (pc_out_DE    ),
        .interr          (interupt     ),
        .inst            (inst_DE      ),
        .rdata           (rdata_csr    ),
        .epc_taken       (epc_taken    ),
        .excep           ( excep       ),
        .epc             ( csr_epc_WB  )
);




    // Data Mem
        data_mem data_mem_i
        (
            .clk            ( clk              ),
            .rd_en          ( rd_en_DM         ),
            .wr_en          ( wr_en_DM         ),
            .addr           ( opr_res_DM       ),
            .mem_acc_mode   ( mem_acc_mode_DM  ),
            .rdata2         ( rdata2           ),
            .rdata          ( rdata            )
        );


    // Writeback MUX
    mux_3x1 wb_mux
    (
        .in_0           ( pc_out_DE + 32'd4 ),
        .in_1           ( opr_res_DM        ),
        .in_2           ( rdata             ),
        .in_3           ( rdata_csr         ),
        .select_line    ( wb_sel_DM         ),
        .out            ( wdata_MW          )
    );


// feedback:
    always_comb 
    begin  
        wdata_DE=wdata_MW; 
        csr_epc_IF=csr_epc_WB;
    end


  hazard_unit hazard_unit_i 
    (
    .rf_en_dm             (rd_en_DM      ),
    .rs1                  (rs1           ),
    .rs2                  (rs2           ),
    .rd_dm                (opr_res_DM    ),
    .forward_a            (Forward_op_A  ),
    .forward_b            (Forward_op_B   ),

    // stalling for data hazards
    .rd_DE                (opr_res_DE    ),
    .sel_wb_DE            ( wb_sel_DE    ),
    .stall_if             ( stall_IF     ),
    .flush_DE             ( flush_DE     ),

    // stalling for control hazards
    .br_taken             (  br_taken_DE )
    );



 

    
endmodule