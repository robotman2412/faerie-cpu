`timescale 1ns/1ns
`default_nettype none

// Copyright © 2024, Julian Scheffers.
// SPDX-License-Identifier: CC-BY-NC-4.0



// Verilog implementation of Faerie's control unit.
module faerie_cu#(
    // Assume a read latency of 1 clock cycle.
    parameter bit sync_read  = 1
)(
    // CPU clock.
    input  wire      clk,
    // Synchronous reset.
    input  wire      rst,
    // Memory read data.
    input  wire[7:0] rdata,
    // ALU / branch mode.
    output wire[3:0] mode,
    
    // Memory write enable.
    output wire we,
    // Memory read enable.
    output wire re,
    // Write to PC if branch condition is satisfied.
    output wire branch,
    // Use PC, not AR, as address.
    output wire pc_addr,
    // Write to low byte of AR.
    output wire set_al,
    // Write to high byte of AR.
    output wire set_ah,
    // Use zero-page addressing mode.
    output wire zp_addr,
    // Reset B register.
    output wire reset_b,
    // Write flags to flags register.
    output wire set_fr,
    // Write ALU result to accumulator.
    output wire set_a,
    // Increment the value of AL for 0page pointer amode.
    output wire inc_al
);
    // Instruction register.
    reg [7:0] insn_reg;
    // Current instruction.
    wire[7:0] insn;
    
    // FSM register.
    reg [7:0] fsm_reg;
    // Current FSM state.
    wire[7:0] fsm = sync_read ? fsm_next : fsm_reg;
    // Next FSM state.
    wire[7:0] fsm_next;
    faerie_cu_fsm fsm_calc(insn, fsm_reg, fsm_next);
    
    // FSM state names.
    wire   insn_ld = fsm == 0;
    wire   addr_1  = fsm[0];
    wire   addr_2  = fsm[1];
    wire   ptr_1   = fsm[2];
    wire   ptr_2   = fsm[3];
    wire   mem_ld  = fsm[4];
    wire   alu     = fsm[5];
    wire   msm_st  = fsm[6];
    assign branch  = fsm[7];
    
    // IR & FSM logic.
    generate
        if (sync_read) begin
            reg p_insn_ld;
            always @(posedge clk) begin
                p_insn_ld <= insn_ld && !rst;
            end
            
            assign insn = p_insn_ld ? rdata : insn_reg;
        end else begin
            assign insn = insn_ld ? rdata : insn_reg;
        end
    endgenerate
    
    always @(posedge clk) begin
        if (rst) begin
            fsm_reg  <= sync_read ? 8'h80 : 0;
            insn_reg <= 'bz;
        end else begin
            fsm_reg  <= fsm_next;
            insn_reg <= insn;
        end
    end
    
    // Control signal generation.
    assign we      = mem_st;
    assign re      = mem_ld || insn_ld || addr_1 || addr_2 || ptr_1 || ptr_2;
    assign pc_addr = insn_ld || (mem_ld && insn[2] && insn[3]);
    assign set_al  = addr_1 || ptr_2;
    assign set_ah  = addr_2 || ptr_1;
    assign zp_addr = ptr_1 || ptr_2 || !(insn[2] || insn[3]);
    assign reset_b = insn_ld;
    assign set_fr  = alu;
    assign set_a   = alu && !insn[0];
    assign inc_al  = ptr_1;
endmodule



// Computes the next state in the control unit FSM.
module faerie_cu_fsm(
    // Current instruction.
    input  wire[7:0] insn,
    // Previous state.
    input  wire[7:0] prev,
    // Next state.
    output wire[7:0] next
);
    wire ild  = prev == 0;
    wire exec = prev[1] | prev[3] | (ild && (insn[2] && insn[3])) | (prev[0] && !insn[2] && !insn[3]);
    
    // Memory stage logic.
    assign next[0] = ild && !(insn[2] && insn[3]);
    assign next[1] = prev[0] && insn[3];
    assign next[2] = prev[0] && !insn[3] && insn[2];
    assign next[3] = prev[2];
    
    // Execution stage logic.
    assign next[4] = (exec && !insn[1] && !(insn[4] | insn[5]));
    assign next[5] = (exec && !insn[1] &&  (insn[4] | insn[5])) | prev[4];
    assign next[6] = (exec &&  insn[1] && !insn[0]);
    assign next[7] = (exec &&  insn[1] &&  insn[0]);
endmodule
