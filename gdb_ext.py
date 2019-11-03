import gdb
import gdb.unwinder

class FrameId(object):
    def __init__(self, sp, pc):
        self.sp = sp
        self.pc = pc

PP_VOID = gdb.lookup_type("void").pointer().pointer()

class ChezUnwinder(gdb.unwinder.Unwinder):

    def __init__(self):
        gdb.unwinder.Unwinder.__init__(self, "ChezUnwinder")

    def __call__(self, frame):
        rip = frame.read_register("rip")
        try:
            block = gdb.block_for_pc(int(rip))
            if block.function.name != "S_generic_invoke":
                return None
        except:
            return None
        # rbp = frame.read_register("rbp")
        rsp = frame.read_register("rsp")
        # print("rbp = {}, rsp = {}, rip = {}".format(rbp, rsp, rip))
        # ptr = rsp + 0x10
        # for i in range(5):
        #     gdb.execute("x/2gx " + hex(ptr))
        #     pc = (ptr + 8).cast(ppvoid).dereference()
        #     print("pc", pc)
        #     try:
        #         block = gdb.block_for_pc(int(pc))
        #         print("block", block)
        #     except RuntimeError:
        #         pass
        #     ptr = ptr.cast(ppvoid).dereference()
        #gdb.execute("x/50gx " + hex(rsp))
        info = frame.create_unwind_info(FrameId(rsp, rip))
        saved_rbp = (rsp + 0x10).cast(PP_VOID).dereference()
        saved_rip = (rsp + 0x18).cast(PP_VOID).dereference()
        #print("s_rbp = {}, s_rip = {}".format(saved_rbp, saved_rip))
        info.add_saved_register("rbp", saved_rbp)
        info.add_saved_register("rip", saved_rip)
        return info


gdb.unwinder.register_unwinder(None, ChezUnwinder())
