acpi -b | awk '{match($4,"[0-9]+%"); print substr($4,RBEGIN,RLENGTH);}'
