`timescale 1ns/1ns
`default_nettype none

// Copyright © 2024, Julian Scheffers.
// SPDX-License-Identifier: CC-BY-NC-4.0



// Cycle-accurate verilog implementation of the Faerie 8-bit CPU.
module faerie_cpu#(
    // Assume a read latency of 1 clock cycle.
    parameter bit       sync_read  = 1,
    // Entrypoint address.
    parameter bit[15:0] entrypoint = 16'hc000
)(
    // CPU clock.
    input  wire clk,
    // Synchronous reset.
    input  wire rst,
    
    // Memory read enable.
    output wire re,
    // Memory write enable.
    output wire we,
    // Memory address.
    output wire[15:0] addr,
    // Memory write data.
    output wire[7:0]  wdata,
    // Memory read data.
    input  wire[7:0]  rdata
);
    // Program counter.
    reg[15:0] pc = entrypoint;
    // Address register.
    reg[15:0] ar;
    // Accumulator.
    reg[7:0]  a;
    // Temporary register.
    reg[7:0]  b;
    // Carry out flag register.
    reg       cf;
    // Zero flag register.
    reg       zf;
    
    // Value of temporary register after forwarding.
    wire[7:0] b_val;
    // Value of address register after forwarding.
    wire[15:0] ar_val;
    
    // ALU mode / branch condition.
    wire[3:0] mode;
    
    // Write to PC if branch condition is satisfied.
    wire branch;
    // Use PC, not AR, as address.
    wire pc_addr;
    // Write to low byte of AR.
    wire set_al;
    // Write to high byte of AR.
    wire set_ah;
    // Use zero-page addressing mode.
    wire zp_addr;
    // Reset B register.
    wire reset_b;
    // Write flags to flags register.
    wire set_fr;
    // Write ALU result to accumulator.
    wire set_a;
    // Increment the value of AL for 0page pointer amode.
    wire inc_al;
    
    // ALU output.
    wire[7:0] q;
    // ALU carry out flag.
    wire      cout;
    
    faerie_cu#(sync_read) cu(
        clk, rst, rdata, mode,
        we, re, branch, pc_addr,
        set_al, set_ah, zp_addr, reset_b,
        set_fr, set_a, inc_al
    );
    
    faerie_alu alu(
        mode, cf,
        a, b,
        q, cout
    );
    
    // Synchronous logic.
    always @(posedge clk) begin
        if (rst) begin
            pc <= entrypoint;
            ar <= 'bx;
            a  <= 0;
            cf <= 0;
            zf <= 0;
        end else begin
            if (branch) begin
                pc <= ar;
            end else if (pc_addr) begin
                pc <= pc + 1;
            end
            if (set_fr) begin
                zf <= q == 0;
                cf <= cout;
            end
            if (set_a) begin
                a  <= q;
            end
            if (reset_b) begin
                b <= 0;
            end
        end
    end
    
    generate
        if (sync_read) begin
            // Control signal buffers.
            reg p_set_al = 0, p_set_ah = 0, p_re = 0;
            always @(posedge clk) begin
                p_set_al <= set_al;
                p_set_ah <= set_ah;
                p_re     <= re;
            end
            
            // B, AR for synchronous read memory.
            assign b_val        = p_re     ? rdata : b;
            assign ar_val[15:8] = p_set_ah ? rdata : ar[15:8];
            assign ar_val[7:0]  = p_set_al ? rdata : ar[7:0];
            always @(posedge clk) begin
                if (rst) begin
                    b        <= 0;
                    ar       <= 'bx;
                    p_re     <= 'bx;
                    p_set_al <= 'bx;
                    p_set_ah <= 'bx;
                end else begin
                    if (p_set_al) begin
                        ar[7:0]  <= rdata;
                    end
                    if (p_set_ah) begin
                        ar[15:8] <= rdata;
                    end
                    if (p_re) begin
                        b <= rdata;
                    end
                end
            end
        end else begin
            // B, AR for asynchronous read memory.
            assign b_val  = b;
            assign ar_val = ar;
            always @(posedge clk) begin
                if (rst) begin
                    b  <= 0;
                    ar <= 'bx;
                end else begin
                    if (set_al) begin
                        ar[7:0]  <= rdata;
                    end
                    if (set_ah) begin
                        ar[15:8] <= rdata;
                    end
                    if (re) begin
                        b <= rdata;
                    end
                end
            end
        end
    endgenerate
endmodule

