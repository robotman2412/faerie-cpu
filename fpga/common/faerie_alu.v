`timescale 1ns/1ns
`default_nettype none

// Copyright © 2024, Julian Scheffers.
// SPDX-License-Identifier: CC-BY-NC-4.0



// Verilog implementation of Faerie's ALU.
module faerie_alu(
    // Mode bits.
    input  wire[3:0] mode,
    // Value of carry flag.
    input  wire      cin,
    // Left-hand side operand.
    input  wire[7:0] a,
    // Right-hand side operand.
    input  wire[7:0] b,
    // Arithmetic result.
    output wire[7:0] q,
    // Carry out.
    output wire      cout
);
    wire      cc      = mode[3];
    wire      sub_and = mode[2];
    wire      shl_xor = mode[1];
    wire      logic_b = mode[0];
    
    wire[7:0] b2      = b ^ (shl_xor ? sub_and * 255 : a);
    wire[7:0] a2      = a ^ (sub_and * 255);
    wire[7:0] andor   = (a2 | b2) ^ (sub_and * 255);
    wire[7:0] logic_q = shl_xor ? andor : b2;
    
    wire      add_cin = cc ? cin : sub_and;
    wire[8:0] add_q   = a + b2 + add_cin;
    
    wire      omux = !sub_and | shl_xor;
    assign    cout = omux ? b[0] : add_q[8];
    wire[7:0] q0   = logic_b ? b : {cc & cin, a[7:1]};
    wire[7:0] q1   = logic_b ? logic_q : add_q[7:0];
    assign    q    = omux ? q1 : q0;
endmodule
