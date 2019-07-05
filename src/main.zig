const std = @import("std");
const assert = std.debug.assert;
const serial = @import("serial.zig");
const mmio = @import("mmio.zig");
const builtin = @import("builtin");
const AtomicOrder = builtin.AtomicOrder;
const debug = @import("debug.zig");

//const jpeg = @import("jpeg_writer.zig");

const multi_threaded = true;

const width = 1024;
const height = 768;
const fov: f32 = std.math.pi / 3.0;
const out_filename = "out.jpg";
const out_quality = 100;

fn vec3(x: f32, y: f32, z: f32) Vec3f {
    return Vec3f{ .x = x, .y = y, .z = z };
}

const Vec3f = Vec3(f32);

fn Vec3(comptime T: type) type {
    return struct {
        const Self = @This();

        x: T,
        y: T,
        z: T,

        fn mul(u: Self, v: Self) T {
            return u.x * v.x + u.y * v.y + u.z * v.z;
        }

        fn mulScalar(u: Self, k: T) Self {
            return vec3(u.x * k, u.y * k, u.z * k);
        }

        fn add(u: Self, v: Self) Self {
            return vec3(u.x + v.x, u.y + v.y, u.z + v.z);
        }

        fn sub(u: Self, v: Self) Self {
            return vec3(u.x - v.x, u.y - v.y, u.z - v.z);
        }

        fn negate(u: Self) Self {
            return vec3(-u.x, -u.y, -u.z);
        }

        fn norm(u: Self) T {
            return std.math.sqrt(u.x * u.x + u.y * u.y + u.z * u.z);
        }

        fn normalize(u: Self) Self {
            return u.mulScalar(1 / u.norm());
        }

        fn cross(u: Vec3f, v: Vec3f) Vec3f {
            return vec3(
                u.y * v.z - u.z * v.y,
                u.z * v.x - u.x * v.z,
                u.x * v.y - u.y * v.x,
            );
        }
    };
}

const Light = struct {
    position: Vec3f,
    intensity: f32,
};

const Material = struct {
    refractive_index: f32,
    albedo: [4]f32,
    diffuse_color: Vec3f,
    specular_exponent: f32,

    pub fn default() Material {
        return Material{
            .refractive_index = 1,
            .albedo = [_]f32{ 1, 0, 0, 0 },
            .diffuse_color = vec3(0, 0, 0),
            .specular_exponent = 0,
        };
    }
};

const Sphere = struct {
    center: Vec3f,
    radius: f32,
    material: Material,

    fn rayIntersect(self: Sphere, origin: Vec3f, direction: Vec3f, t0: *f32) bool {
        const l = self.center.sub(origin);
        const tca = l.mul(direction);
        const d2 = l.mul(l) - tca * tca;

        if (d2 > self.radius * self.radius) {
            return false;
        }

        const thc = std.math.sqrt(self.radius * self.radius - d2);
        t0.* = tca - thc;
        const t1 = tca + thc;
        if (t0.* < 0) t0.* = t1;
        return t0.* >= 0;
    }
};

fn reflect(i: Vec3f, normal: Vec3f) Vec3f {
    return i.sub(normal.mulScalar(2).mulScalar(i.mul(normal)));
}

fn refract(i: Vec3f, normal: Vec3f, refractive_index: f32) Vec3f {
    var cosi = -std.math.max(-1, std.math.min(1, i.mul(normal)));
    var etai: f32 = 1;
    var etat = refractive_index;

    var n = normal;
    if (cosi < 0) {
        cosi = -cosi;
        std.mem.swap(f32, &etai, &etat);
        n = normal.negate();
    }

    const eta = etai / etat;
    const k = 1 - eta * eta * (1 - cosi * cosi);
    return if (k < 0) vec3(0, 0, 0) else i.mulScalar(eta).add(n.mulScalar(eta * cosi - std.math.sqrt(k)));
}

fn sceneIntersect(origin: Vec3f, direction: Vec3f, spheres: []const Sphere, hit: *Vec3f, normal: *Vec3f, material: *Material) bool {
    var spheres_dist: f32 = std.math.f32_max;
    for (spheres) |s| {
        var dist_i: f32 = undefined;
        if (s.rayIntersect(origin, direction, &dist_i) and dist_i < spheres_dist) {
            spheres_dist = dist_i;
            hit.* = origin.add(direction.mulScalar(dist_i));
            normal.* = hit.sub(s.center).normalize();
            material.* = s.material;
        }
    }

    // Floor plane
    var checkerboard_dist: f32 = std.math.f32_max;
    if (std.math.fabs(direction.y) > 1e-3) {
        const d = -(origin.y + 4) / direction.y;
        const pt = origin.add(direction.mulScalar(d));
        if (d > 0 and std.math.fabs(pt.x) < 10 and pt.z < -10 and pt.z > -30 and d < spheres_dist) {
            checkerboard_dist = d;
            hit.* = pt;
            normal.* = vec3(0, 1, 0);

            const diffuse = @floatToInt(i32, 0.5 * hit.x + 1000) + @floatToInt(i32, 0.5 * hit.z);
            const diffuse_color = if (@mod(diffuse, 2) == 1) vec3(1, 1, 1) else vec3(1, 0.7, 0.3);
            material.diffuse_color = diffuse_color.mulScalar(0.3);
        }
    }

    return std.math.min(spheres_dist, checkerboard_dist) < 1000;
}

fn castRay(origin: Vec3f, direction: Vec3f, spheres: []const Sphere, lights: []const Light, depth: i32) Vec3f {
    var point: Vec3f = undefined;
    var normal: Vec3f = undefined;
    var material = Material.default();

    if (depth > 4 or !sceneIntersect(origin, direction, spheres, &point, &normal, &material)) {
        return vec3(0.2, 0.7, 0.8); // Background color
    }

    const reflect_dir = reflect(direction, normal).normalize();
    const refract_dir = refract(direction, normal, material.refractive_index).normalize();

    const nn = normal.mulScalar(1e-3);
    const reflect_origin = if (reflect_dir.mul(normal) < 0) point.sub(nn) else point.add(nn);
    const refract_origin = if (refract_dir.mul(normal) < 0) point.sub(nn) else point.add(nn);

    const reflect_color = castRay(reflect_origin, reflect_dir, spheres, lights, depth + 1);
    const refract_color = castRay(refract_origin, refract_dir, spheres, lights, depth + 1);

    var diffuse_light_intensity: f32 = 0;
    var specular_light_intensity: f32 = 0;

    for (lights) |l| {
        const light_dir = l.position.sub(point).normalize();
        const light_distance = l.position.sub(point).norm();

        const shadow_origin = if (light_dir.mul(normal) < 0) point.sub(nn) else point.add(nn);

        var shadow_pt: Vec3f = undefined;
        var shadow_n: Vec3f = undefined;
        var _unused: Material = undefined;
        if (sceneIntersect(shadow_origin, light_dir, spheres, &shadow_pt, &shadow_n, &_unused) and shadow_pt.sub(shadow_origin).norm() < light_distance) {
            continue;
        }

        diffuse_light_intensity += l.intensity * std.math.max(0, light_dir.mul(normal));
        specular_light_intensity += std.math.pow(f32, std.math.max(0, -reflect(light_dir.negate(), normal).mul(direction)), material.specular_exponent) * l.intensity;
    }

    const p1 = material.diffuse_color.mulScalar(diffuse_light_intensity * material.albedo[0]);
    const p2 = vec3(1, 1, 1).mulScalar(specular_light_intensity).mulScalar(material.albedo[1]);
    const p3 = reflect_color.mulScalar(material.albedo[2]);
    const p4 = refract_color.mulScalar(material.albedo[3]);
    return p1.add(p2.add(p3.add(p4)));
}

const RenderContext = struct {
    pixmap: []u8,
    start: usize,
    end: usize,
    spheres: []const Sphere,
    lights: []const Light,
};

fn renderFramebufferSegment(context: RenderContext) void {
    var j: usize = context.start;
    while (j < context.end) : (j += 1) {
        var i: usize = 0;
        while (i < width) : (i += 1) {
            const x = (2 * (@intToFloat(f32, i) + 0.5) / width - 1) * std.math.tan(fov / 2.0) * width / height;
            const y = -(2 * (@intToFloat(f32, j) + 0.5) / height - 1) * std.math.tan(fov / 2.0);

            const direction = vec3(x, y, -1).normalize();
            var c = castRay(vec3(0, 0, 0), direction, context.spheres, context.lights, 0);

            var max = std.math.max(c.x, std.math.max(c.y, c.z));
            if (max > 1) c = c.mulScalar(1 / max);

            const T = @typeInfo(Vec3f).Struct;
            inline for (T.fields) |field, k| {
                const pixel = @floatToInt(u8, 255 * std.math.max(0, std.math.min(1, @field(c, field.name))));
                context.pixmap[3 * (i + j * width) + k] = pixel;
            }
        }
    }
}

fn renderMulti(allocator: *std.mem.Allocator, spheres: []const Sphere, lights: []const Light) !void {
    var pixmap = std.ArrayList(u8).init(allocator);
    defer pixmap.deinit();
    try pixmap.resize(3 * width * height);

    const cpu_count = try std.Thread.cpuCount();
    const batch_size = height / cpu_count;

    var threads = std.ArrayList(*std.Thread).init(allocator);
    defer threads.deinit();

    var j: usize = 0;
    while (j < height) : (j += batch_size) {
        const context = RenderContext{
            .pixmap = pixmap.toSlice(),
            .start = j,
            .end = j + batch_size,
            .spheres = spheres,
            .lights = lights,
        };

        try threads.append(try std.Thread.spawn(context, renderFramebufferSegment));
    }

    for (threads.toSliceConst()) |thread| {
        thread.wait();
    }

    try jpeg.writeToFile(out_filename, width, height, 3, pixmap.toSliceConst(), out_quality);
}

fn render(allocator: *std.mem.Allocator, spheres: []const Sphere, lights: []const Light) !void {
    var pixmap = std.ArrayList(u8).init(allocator);
    defer pixmap.deinit();
    try pixmap.resize(3 * width * height);

    var j: usize = 0;
    while (j < height) : (j += 1) {
        var i: usize = 0;
        while (i < width) : (i += 1) {
            const x = (2 * (@intToFloat(f32, i) + 0.5) / width - 1) * std.math.tan(fov / 2.0) * width / height;
            const y = -(2 * (@intToFloat(f32, j) + 0.5) / height - 1) * std.math.tan(fov / 2.0);

            const direction = vec3(x, y, -1).normalize();
            var c = castRay(vec3(0, 0, 0), direction, spheres, lights, 0);

            var max = std.math.max(c.x, std.math.max(c.y, c.z));
            if (max > 1) c = c.mulScalar(1 / max);

            const T = @typeInfo(Vec3f).Struct;
            inline for (T.fields) |field, k| {
                const pixel = @floatToInt(u8, 255 * std.math.max(0, std.math.min(1, @field(c, field.name))));
                pixmap.set(3 * (i + j * width) + k, pixel);
            }
        }
    }

    try jpeg.writeToFile(out_filename, width, height, 3, pixmap.toSliceConst(), out_quality);
}


// The linker will make the address of these global variables equal
// to the value we are interested in. The memory at the address
// could alias any uninitialized global variable in the kernel.
extern var __bss_start: u8;
extern var __bss_end: u8;
extern var __end_init: u8;

comptime {
    // .text.boot to keep this in the first portion of the binary
    // Note: this code cannot be changed via the bootloader.
    asm (
        \\.section .text.boot
        \\.globl _start
        \\_start:
        \\ #mrs x0,mpidr_el1 #TODO: find RISC-V equivalent
        \\ lui x1, 0xC1000
        \\ #bic x0,x0,x1
        \\ #cbz x0,master
        \\ j hang
        \\master:
        \\#la sp, 0x80000000
        \\ lui sp, 0x80000
        \\ call kernel_main
        \\hang:
        \\ #wfe #TODO: find RISC-V equivalent
        \\ j hang
    );
}

pub fn panic(message: []const u8, stack_trace: ?*builtin.StackTrace) noreturn {
    debug.panic(stack_trace, "KERNEL PANIC: {}", message);
}

export fn kernel_main() linksection(".text.main") noreturn {
    // clear .bss
    @memset((*volatile [1]u8)(&__bss_start), 0, @ptrToInt(&__bss_end) - @ptrToInt(&__bss_start));

    serial.init();
    serial.log("ClashOS 0.0\n");

    //while (true) {
    //    if (fb_init()) {
    //        break;
    //    } else |_| {
    //        panic("Unable to initialize framebuffer", null);
    //    }
    //}

    //serial.log("Screen size: {}x{}\n", fb_info.width, fb_info.height);

    //fb_clear(&color_blue);

    serialLoop();
}

const build_options = @import("build_options");
const bootloader_code align(@alignOf(std.elf.Elf64_Ehdr)) = @embedFile("../" ++ build_options.bootloader_exe_path);

fn serialLoop() noreturn {
    const boot_magic = [_]u8{ 6, 6, 6 };
    var boot_magic_index: usize = 0;
    while (true) {
        const byte = serial.readByte();
        if (byte == boot_magic[boot_magic_index]) {
            boot_magic_index += 1;
            if (boot_magic_index != boot_magic.len)
                continue;

            // It's time to receive the new kernel. First
            // we skip over the .text.boot bytes, verifying that they
            // are unchanged.
            const new_kernel_len = serial.in.readIntLittle(u32) catch unreachable;
            serial.log("New kernel image detected, {Bi2}\n", new_kernel_len);
            const text_boot = @intToPtr([*]allowzero const u8, 0)[0..@ptrToInt(&__end_init)];
            for (text_boot) |text_boot_byte, byte_index| {
                const new_byte = serial.readByte();
                if (new_byte != text_boot_byte) {
                    debug.panic(
                        @errorReturnTrace(),
                        "new_kernel[{}] expected: 0x{x} actual: 0x{x}",
                        byte_index,
                        text_boot_byte,
                        new_byte,
                    );
                }
            }
            const start_addr = @ptrToInt(kernel_main);
            const bytes_left = new_kernel_len - start_addr;
            var pad = start_addr - text_boot.len;
            while (pad > 0) : (pad -= 1) {
                _ = serial.readByte();
            }

            // Next we copy the bootloader code to the correct memory address,
            // and then jump to it.
            // Read the ELF
            var bootloader_code_ptr = ([*]const u8)(&bootloader_code); // TODO remove workaround `var`
            const ehdr = @ptrCast(*const std.elf.Elf64_Ehdr, bootloader_code_ptr);
            var phdr_addr = bootloader_code_ptr + ehdr.e_phoff;
            var phdr_i: usize = 0;
            while (phdr_i < ehdr.e_phnum) : ({
                phdr_i += 1;
                phdr_addr += ehdr.e_phentsize;
            }) {
                const this_ph = @ptrCast(*const std.elf.Elf64_Phdr, phdr_addr);
                switch (this_ph.p_type) {
                    std.elf.PT_LOAD => {
                        const src_ptr = bootloader_code_ptr + this_ph.p_offset;
                        const src_len = this_ph.p_filesz;
                        const dest_ptr = @intToPtr([*]u8, this_ph.p_vaddr);
                        const dest_len = this_ph.p_memsz;
                        const pad_len = dest_len - src_len;
                        const copy_len = dest_len - pad_len;
                        @memcpy(dest_ptr, src_ptr, copy_len);
                        @memset(dest_ptr + copy_len, 0, pad_len);
                    },
                    std.elf.PT_GNU_STACK => {}, // ignore
                    else => debug.panic(
                        @errorReturnTrace(),
                        "unexpected ELF Program Header load type: {}",
                        this_ph.p_type,
                    ),
                }
            }
            serial.log("Loading new image...\n");
            asm volatile (
                \\li sp, 0x08000000
                \\call bootloader_main
                            :
                : [arg0] "{x0}" (start_addr),
                  [arg1] "{x1}" (bytes_left)
            );
            unreachable;
        }
        switch (byte) {
            '\r' => {
                serial.writeText("\n");
            },
            else => serial.writeByte(byte),
        }
    }
}

const color_red = Color{ .red = 255, .green = 0, .blue = 0 };
const color_green = Color{ .red = 0, .green = 255, .blue = 0 };
const color_blue = Color{ .red = 0, .green = 0, .blue = 255 };

var fb_info: FbInfo = undefined;

const FbInfo = struct {
    // Stuff about the pixel frame buffer
    width: usize,
    height: usize,
    pitch: usize, //BCM2836 has this separate, so we use this instead of witdh
    ptr: [*]volatile u8,
    size: usize,
};

const Bcm2836FrameBuffer = packed struct {
    width: usize, // Width of the frame buffer (pixels)
    height: usize, // Height of the frame buffer
    vwidth: usize, // Simplest thing to do is to set vwidth = width
    vheight: usize, // Simplest thing to do is to set vheight = height
    pitch: usize, // GPU fills this in, set to zero
    depth: usize, // Bits per pixel, set to 24
    x: usize, // Offset in x direction. Simplest thing to do is set to zero
    y: usize, // Offset in y direction. Simplest thing to do is set to zero
    pointer: usize, // GPU fills this in to be a pointer to the frame buffer
    size: usize, // GPU fills this in
};

fn fb_init() error{}!void {
    //serial.log("Initializing USB...\n");
    //%%usb.init();
    serial.log("Initializing frame buffer...\n");

    // We need to put the frame buffer structure somewhere with the lower 4 bits zero.
    // 0x400000 is a convenient place not used by anything, and with sufficient alignment
    const fb = @intToPtr(*volatile Bcm2836FrameBuffer, 0x400000);

    const width = 800;
    const height = 600;

    @fence(AtomicOrder.SeqCst);
    fb.width = width;
    fb.height = height;
    fb.vwidth = width;
    fb.vheight = height;
    fb.pitch = 0;
    fb.depth = 24;
    fb.x = 0;
    fb.y = 0;
    fb.pointer = 0;
    fb.size = 0;

    //// Tell the GPU the address of the structure
    //mbox_write(ArmToVc(@ptrToInt(fb)));

    //// Wait for the GPU to respond, and get its response
    //const response = mbox_read();
    //if (response != 0) return error.NonZeroFrameBufferResponse;
    //if (fb.pointer == 0) return error.NullFrameBufferPointer;

    //fb_info.ptr = @intToPtr([*]u8, VcToArm(fb.pointer));
    //fb_info.size = fb.size;
    //fb_info.width = fb.width;
    //fb_info.height = fb.height;
    //fb_info.pitch = fb.pitch;
}

fn fb_clear(color: *const Color) void {
    {
        var y: usize = 0;
        while (y < fb_info.height) : (y += 1) {
            {
                var x: usize = 0;
                while (x < fb_info.width) : (x += 1) {
                    const offset = y * fb_info.pitch + x * 3;
                    fb_info.ptr[offset] = color.red;
                    fb_info.ptr[offset + 1] = color.green;
                    fb_info.ptr[offset + 2] = color.blue;
                }
            }
        }
    }
    @fence(AtomicOrder.SeqCst);
}

const Color = struct {
    red: u8,
    green: u8,
    blue: u8,
};

const PERIPHERAL_BASE = 0x3F000000; // Base address for all peripherals

// This is the base address for the mailbox registers
// Actually, there's more than one mailbox, but this is the one we care about.
const MAIL_BASE = PERIPHERAL_BASE + 0xB880;

// Registers from mailbox 0 that we use
const MAIL_READ = MAIL_BASE + 0x00; // We read from this register
const MAIL_WRITE = MAIL_BASE + 0x20; // This is where we write to; it is actually the read/write of the other mailbox
const MAIL_STATUS = MAIL_BASE + 0x18; // Status register for this mailbox
const MAIL_CONFIG = MAIL_BASE + 0x1C; // we don't actually use this, but it exists

// This bit is set in the status register if there is no space to write into the mailbox
const MAIL_FULL = 0x80000000;
// This bit is set if there is nothing to read from the mailbox
const MAIL_EMPTY = 0x40000000;

const MAIL_FB = 1; // The frame buffer uses channel 1

fn mbox_write(v: u32) void {
    // wait for space
    while (mmio.read(MAIL_STATUS) & MAIL_FULL != 0) {}
    // Write the value to the frame buffer channel
    mmio.write(MAIL_WRITE, MAIL_FB | (v & 0xFFFFFFF0));
}

fn mbox_read() u32 {
    while (true) {
        // wait for data
        while (mmio.read(MAIL_STATUS) & MAIL_EMPTY != 0) {}
        const result = mmio.read(MAIL_READ);

        // Loop until we received something from the
        // frame buffer channel
        if ((result & 0xf) == MAIL_FB)
            return result & 0xFFFFFFF0;
    }
}

fn ArmToVc(addr: usize) usize {
    // Some things (e.g: the GPU) expect bus addresses, not ARM physical
    // addresses
    return addr + 0xC0000000;
}

fn VcToArm(addr: usize) usize {
    // Go the other way to ArmToVc
    return addr - 0xC0000000;
}
