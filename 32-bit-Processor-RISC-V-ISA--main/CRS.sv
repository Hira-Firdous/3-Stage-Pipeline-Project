module CRS
(
    input  logic        clk,
    input logic         csr_reg_wrMW,  // control signal for CSR register file write
    input logic         csr_reg_rdMW,   // control signal for CSR register file read
    input logic         is_mret,
    input  logic [31:0] addr,
    input  logic [31:0] data,
    input  logic [31:0] pc,
    input  logic [31:0] interr,
    input  logic [31:0] inst,
    output logic        epc_taken,
    output logic [31:0] rdata,
    output logic        excep,
    output logic [31:0] epc
);

    logic [31:0] csr_mem [5];

    
    //assign epc=pc;
    //logic [11:0] mcr;
    //logic [31:0] mtvec;
    //logic [31:0] mie;
    //logic [31:0] mstatus;
    //logic [31:0] mip;
    //logic [31:0] mepc;
    //assign mcr    = inst[31:20];
    //assign mie   = csr_mem [0];
    //assign mstatus   = csr_mem [1];
    //assign mip   = csr_mem [2];
    //assign mepc   = csr_mem [3];
    //assign mtvec   = csr_mem [4];

    // for storing the cause 
    reg [31:0] mip;
    reg [31:0] mie;
    reg [31:0] mstatus;
    reg [31:0] mcause;
    reg [31:0] mtvec;
    reg [31:0] mepc;
    reg [31:0] cause;
    

    
    // asynchronous read
    always_comb 
    begin
        

        if (is_mret)
            case(inst[31:20])
                12'h341: rdata=csr_mem [3]; //mepc;
            endcase
        if( csr_reg_rdMW)
        case(inst[31:20])
            12'h304: rdata=csr_mem [0]; //mie;
            12'h300: rdata=csr_mem [1]; //mstatus;
            12'h344: rdata=csr_mem [2]; //mip;
            12'h305: rdata=csr_mem [4]; //mtvec
                
            
        endcase
            
       
    end
    

    // synchronus write
    always_ff @(posedge clk) 
    begin
        if (interr)
        begin
            csr_mem [1][3] <= 1'b1;        
            csr_mem [2][7] <= 1'b1;        
            csr_mem [0][7] <= 1'b1;        
        end 
        if (is_mret)
            case(inst[31:20])
                12'h341: csr_mem [3]<=data;//mepc;
            endcase
        if(csr_reg_wrMW)
        begin
            case(inst[31:20])
                12'h304: csr_mem [0]<=data; //mie
                12'h300: csr_mem [1]<=data; //mstatus
                12'h344: csr_mem [2]<=data;// mip
                12'h305: csr_mem [4]<=data; //mtvec
            endcase
        end

          //mstatus           mip               mie
        if(csr_mem [1][3] && (csr_mem [2][7] && csr_mem [0][7]))
            begin
                excep = 1'b1;
                csr_mem [3]   <= pc; // mepc 
                epc   <= csr_mem [4];// mtvec
                epc_taken <= 1;
                csr_mem [0]    <= 0; //mie
                mcause <= cause;
            end
            else if (is_mret)
                begin
                    epc <= csr_mem [3]; //mepc
                    epc_taken <= 1;
                    mcause <= 0;
                end
            else
            begin
                epc_taken <= 0;
                excep = 1'b1;
    end
    end

endmodule
