% This header defines the constants used
% to describe entries used to commit to the
% backend storage.

-record(entry, {
                % Which protocol was used for the storage (ascii, binary)
                protocol = ascii,
                key,
                value,
                size,
                version, % undefined if not set, used for cas
                flags, % Opaque set of flags
                expiration = infinity % Timestamp
            }).


