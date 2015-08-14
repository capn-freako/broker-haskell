#include <stdio.h>
#include "../bro/aux/broker/broker/broker.h"

int main (int argc, char* argv[]) {
    int res = broker_init(0);
    broker_endpoint* ep1 = broker_endpoint_create_with_flags("ep1", 3);
    broker_endpoint* ep2 = broker_endpoint_create_with_flags("ep2", 3);
    broker_string*    bs = broker_string_create("");
    broker_message_queue* q = broker_message_queue_create(bs, ep2);
    broker_peering*    p = broker_endpoint_peer_locally(ep2, ep1);
    const broker_outgoing_connection_status_queue* ocsq =
        broker_endpoint_outgoing_connection_status(ep2);
    broker_deque_of_outgoing_connection_status_delete(
       broker_outgoing_connection_status_queue_need_pop(ocsq));
    broker_string*  msg_str = broker_string_create("Hello, World!\n");
    broker_string*    topic = broker_string_create("test");
    broker_data*        msg = broker_data_from_string(msg_str);
    broker_vector*      vec = broker_vector_create();
    res = broker_vector_insert(vec, msg, 0L);
    res = broker_endpoint_send(ep1, topic, vec);
    broker_deque_of_message* msg_list = broker_message_queue_need_pop(q);
    size_t num_msgs = broker_deque_of_message_size(msg_list);
    printf("There are %ld messages.\n", num_msgs);
}

