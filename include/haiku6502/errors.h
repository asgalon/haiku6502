//
// Created by Peter Köllner on 19/2/26.
//

#ifndef HAIKU6502_ERRORS_H
#define HAIKU6502_ERRORS_H

namespace haiku6502 {
    class Error : public std::exception {
    public:
        enum Type {
            BAD_FILE = 1,
            IO_ERROR,
            VALUE_OUT_OF_RANGE,
            PERIPHERAL_ERROR,
            UNEXPECTED = -1
        };

        Type type;

        explicit Error(Type t) : type(t) {

        }
    };
}
#endif //HAIKU6502_ERRORS_H