module hazard_unit 
(
    // forwarding
    input  logic        rf_en_dm,
    input  logic [ 4:0] rs1,
    input  logic [ 4:0] rs2,
    input  logic [ 31:0] rd_dm,
    output logic [ 1:0] forward_a,
    output logic [ 1:0] forward_b,

    // stalling for data hazards
    input  logic [ 31:0] rd_DE,
    input  logic [ 1:0] sel_wb_DE,
    output logic        stall_if,
    output logic        flush_DE,

    // stalling for control hazards
    input  logic        br_taken
);

    logic stall_lw;

    always_comb
    begin
        if (((rs1 == rd_dm) & rf_en_dm) & (rs1 != 0))
        begin 
            forward_a = 2'b10;
        end
        else 
        begin
            forward_a = 2'b00;
        end
    end

    always_comb
    begin
        if (((rs2 == rd_dm) & rf_en_dm) & (rs2 != 0))
        begin 
            forward_b = 2'b10;
        end
        else 
        begin
            forward_b = 2'b00;
        end
    end

    always_comb
    begin
        if((sel_wb_DE == 2'b01) & ((rs1 == rd_DE) | (rs2 == rd_DE)))
        begin
            stall_lw = 1'b1;
        end
        else
        begin
            stall_lw = 1'b0;
        end
    end

    assign stall_IF = stall_lw;

    assign flush_DE = (stall_lw | br_taken);

endmodule