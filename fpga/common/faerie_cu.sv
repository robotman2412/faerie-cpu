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
    input  logic      clk,
    // Synchronous reset.
    input  logic      rst,
    // Memory read data.
    input  logic[7:0] rdata,
    
    // Memory write enable.
    output logic we,
    // Memory read enable.
    output logic re,
    // Write to PC if branch condition is satisfied.
    output logic branch,
    // Use PC, not AR, as address.
    output logic pc_addr,
    // Write to low byte of AR.
    output logic set_al,
    // Write to high byte of AR.
    output logic set_ah,
    // Use zero-page addressing mode.
    output logic zp_addr,
    // Reset B register.
    output logic reset_b,
    // Write flags to flags register.
    output logic set_fr,
    // Write ALU result to accumulator.
    output logic set_a,
    // Increment the value of AL for 0page pointer amode.
    output logic inc_al
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
    
    // Control signal generation.
    assign we      = mem_ld;
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
    input  logic[7:0] insn,
    // Previous state.
    input  logic[7:0] prev,
    // Next state.
    output logic[7:0] next
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
