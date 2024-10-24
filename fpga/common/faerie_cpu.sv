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
    input  logic clk,
    // Synchronous reset.
    input  logic rst,
    
    // Memory read enable.
    output logic re,
    // Memory write enable.
    output logic we,
    // Memory address.
    output logic[15:0] addr,
    // Memory write data.
    output logic[7:0]  wdata,
    // Memory read data.
    input  logic[7:0]  rdata
);
    // Program counter.
    reg[15:0] pc;
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
endmodule
