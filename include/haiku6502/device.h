//
// Created by Peter Köllner on 27/1/26.
//

#ifndef HAIKU6502_DEVICE_H
#define HAIKU6502_DEVICE_H

namespace haiku6502 {
    class Device {
    protected:
        // init is called when peripheral is added to the engine
        virtual void device_init()  = 0;

        // close is called when peripheral is removed from the engine
        virtual void device_close() = 0;
    public:
        virtual ~Device() = default;

        virtual void pre_cycle() {}
        virtual void post_cycle() {}
    };
}
#endif //HAIKU6502_DEVICE_H