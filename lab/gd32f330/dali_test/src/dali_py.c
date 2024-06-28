#define HOST "localhost"
#define PORT 55825

typedef struct DaliServer {
    char* host;
    int port;
} DaliServer;

void set_search_addr(DaliServer* i, uint32_t addr) {
    uint8_t high = (addr >> 16) & 0xff;
    uint8_t middle = (addr >> 8) & 0xff;
    uint8_t low = addr & 0xff;

    i->send(SetSearchAddrH, high);
    i->send(SetSearchAddrM, middle);
    i->send(SetSearchAddrL, low);
}

uint32_t find_next(DaliServer* i, uint32_t low, uint32_t high) {
    printf("Searching from %u to %u...\n", low, high);
    if (low == high) {
        set_search_addr(i, low);
        bool value = i->send(Compare);

        if (value) {
            printf("Found ballast at %u; withdrawing it...\n", low);
            i->send(Withdraw);
            return low;
        }
        return -1;
    }

    set_search_addr(i, high);
    bool response = i->send(Compare);

    if (response) {
        uint32_t midpoint = (low + high) / 2;
        return find_next(i, low, midpoint) != -1 || find_next(i, midpoint + 1, high) != -1;
    }

    return -1;
}

int find_ballasts(DaliServer* i) {
    uint32_t* ballasts = NULL;
    uint32_t num_ballasts = 0;

    i->send(Terminate);
    i->send(Initialise, true, NULL);
    i->send(Randomise);
    sleep(1); // Randomise may take up to 100ms

    uint32_t low = 0;
    uint32_t high = 0xffffff;
    while (low != -1) {
        low = find_next(i, low, high);
        if (low != -1) {
            ballasts = realloc(ballasts, sizeof(uint32_t) * (num_ballasts + 1));
            ballasts[num_ballasts] = low;
            num_ballasts++;
            low++;
        }
    }

    i->send(Terminate);
    return num_ballasts;
}

int main(int argc, char** argv) {

    printf("Looking for ballast devices at %s:%d\n", host, port);

    uint32_t* ballasts = find_ballasts(&d);

    printf("%u ballasts found:\n", ballasts);
    for (int i = 0; i < ballasts; i++) {
        printf("%u\n", ballasts[i]);
    }

    free(ballasts);

    return 0;
}