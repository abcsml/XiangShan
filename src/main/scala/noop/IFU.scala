package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._

trait HasResetVector {
  val resetVector = 0x80100000L//TODO: set reset vec
}

class IFU extends NOOPModule with HasResetVector {
  val io = IO(new Bundle {
    val imem = new SimpleBusUC(userBits = AddrBits)
    val pc = Input(UInt(AddrBits.W))
    val out = Decoupled(new CtrlFlowIO)
    val redirect = Flipped(new RedirectIO)
    val flushVec = Output(UInt(4.W))
    val bpFlush = Output(Bool())
  })

  // pc
  val pc = RegInit(resetVector.U(AddrBits.W))
  val pcUpdate = io.redirect.valid || io.imem.req.fire()
  val snpc = pc + 4.U  // sequential next pc

  val bp1 = Module(new BPU1)
  // predicted next pc
  val pnpc = bp1.io.out.target
  val npc = Mux(io.redirect.valid, io.redirect.target, Mux(bp1.io.out.valid, pnpc, snpc))

  bp1.io.in.pc.valid := io.imem.req.fire() // only predict when Icache accepts a request
  bp1.io.in.pc.bits := npc  // predict one cycle early
  bp1.io.flush := io.redirect.valid

  when (pcUpdate) { pc := npc }

  io.flushVec := Mux(io.redirect.valid, "b1111".U, 0.U)
  io.bpFlush := false.B

  io.imem := DontCare
  io.imem.req.valid := io.out.ready
  io.imem.req.bits.addr := Cat(pc(AddrBits-1,2),0.U(2.W))//cache will treat it as Cat(pc(63,3),0.U(3.W)) 
  io.imem.req.bits.size := "b11".U
  io.imem.req.bits.cmd := SimpleBusCmd.read
  io.imem.req.bits.user := npc
  io.imem.resp.ready := io.out.ready || io.flushVec(0)

  io.out.bits := DontCare
  io.out.bits.pc := io.pc
    //inst path only uses 32bit inst, get the right inst according to pc(2)
  io.out.bits.instr := (if (XLEN == 64) io.imem.resp.bits.rdata.asTypeOf(Vec(2, UInt(32.W)))(io.pc(2))
                       else io.imem.resp.bits.rdata)
  io.out.bits.pnpc := io.imem.resp.bits.user
  io.out.valid := io.imem.resp.valid && !io.flushVec(0)

  BoringUtils.addSource(BoolStopWatch(io.imem.req.valid, io.imem.resp.fire()), "perfCntCondMimemStall")
  BoringUtils.addSource(io.flushVec.orR, "perfCntCondMifuFlush")
}