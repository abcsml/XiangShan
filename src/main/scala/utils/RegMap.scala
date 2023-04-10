/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utils

import chisel3._
import chisel3.util._

object RegMap {
  def Unwritable = null
  def apply(addr: Int, reg: UInt, wfn: UInt => UInt = (x => x)) = (addr, (reg, wfn))
  def generate(mapping: Map[Int, (UInt, UInt => UInt)], raddr: UInt, rdata: UInt,
    waddr: UInt, wen: Bool, wdata: UInt, wmask: UInt):Unit = {
    val chiselMapping = mapping.map { case (a, (r, w)) => (a.U, r, w) }
    rdata := LookupTree(raddr, chiselMapping.map { case (a, r, w) => (a, r) })
    chiselMapping.map { case (a, r, w) =>
      if (w != null) when (wen && waddr === a) { r := w(MaskData(r, wdata, wmask)) }
    }
  }
  def generate(mapping: Map[Int, (UInt, UInt => UInt)], addr: UInt, rdata: UInt,
    wen: Bool, wdata: UInt, wmask: UInt):Unit = generate(mapping, addr, rdata, addr, wen, wdata, wmask)
}

object MaskedRegMap { // TODO: add read mask
  def Unwritable = null
  def NoSideEffect: UInt => UInt = (x=>x)
  def WritableMask = Fill(64, true.B)
  def UnwritableMask = 0.U(64.W)
  /**
    * 用来封装CSR寄存器，交由generate函数连线
    *
    * @param addr   CSR寄存器地址（例：Mstatus地址为0x300）
    * @param reg    寄存器本体
    * @param wmask  写掩码，64位，为1表示此位可写
    * @param wfn    写函数，写入数据要经过此函数处理（例：不可写用Unwritable，无需处理用NoSideEffect）
    * @param rmask  读掩码，64位，为1表示此位可读，为0则此位永远返回0
    * @param rfn    读函数，读出数据要经过此函数 rfn(reg & rmask)
    * @return       返回map，由generate使用
    */
  def apply(addr: Int, reg: UInt,
            wmask: UInt = WritableMask, wfn: UInt => UInt = (x => x),
            rmask: UInt = WritableMask, rfn: UInt => UInt = x=>x
           ): (Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)) = (addr, (reg, wmask, wfn, rmask, rfn))
  /**
    * 对应上面apply函数，配合使用
    *
    * @param mapping  输入，由apply产生的mapping
    * @param raddr    输入，读地址
    * @param rdata    输出，读出的数据（无延迟）
    * @param waddr    输入，写地址
    * @param wen      输入，写有效
    * @param wdata    输入，写地址
    */
  def generate(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)], raddr: UInt, rdata: UInt,
    waddr: UInt, wen: Bool, wdata: UInt):Unit = {
    val chiselMapping = mapping.map { case (a, (r, wm, w, rm, rfn)) => (a.U, r, wm, w, rm, rfn) }
    rdata := LookupTree(raddr, chiselMapping.map { case (a, r, _, _, rm, rfn) => (a, rfn(r & rm)) })
    val wdata_reg = RegEnable(wdata, wen)
    chiselMapping.foreach { case (a, r, wm, w, _, _) =>
      if (w != null && wm != UnwritableMask) {
        // Warning: this RegMap adds a RegNext for write to reduce fanout
        // the w must be pure function without side effects
        val wen_reg = RegNext(wen && waddr === a)
        when (wen_reg) { r := w(MaskData(r, wdata_reg, wm)) }
      }
    }
  }
  def isIllegalAddr(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)], addr: UInt):Bool = {
    val illegalAddr = Wire(Bool())
    illegalAddr := LookupTreeDefault(addr, true.B, mapping.toSeq.sortBy(_._1).map { case (a, _) => (a.U, false.B) })
    illegalAddr
  }
  def generate(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)], addr: UInt, rdata: UInt,
    wen: Bool, wdata: UInt):Unit = generate(mapping, addr, rdata, addr, wen, wdata)
}
