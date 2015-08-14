#include <stdio.h>
#include <stdlib.h>
#include "../bro/aux/broker/broker/broker.h"

void my_exit (char* msg) {
    printf("%s", msg);
    printf("\n");
    exit(-1);
}

int main (int argc, char* argv[]) {
    int res = broker_init(0);
    if(res) my_exit("broker_init() failed!");

    broker_endpoint* ep1 = broker_endpoint_create_with_flags("ep1", 3);
    if(!ep1) my_exit("Failed to create first endpoint!");

    broker_endpoint* ep2 = broker_endpoint_create_with_flags("ep2", 3);
    if(!ep2) my_exit("Failed to create second endpoint!");

    broker_string*    bs = broker_string_create("");
    if(!bs) my_exit("Failed to create topic string!");

    broker_message_queue* q = broker_message_queue_create(bs, ep2);
    if(!q) my_exit("Failed to create message queue!");

    broker_peering*    p = broker_endpoint_peer_locally(ep2, ep1);
    if(!p) my_exit("Failed to create peering!");

    const broker_outgoing_connection_status_queue* ocsq =
        broker_endpoint_outgoing_connection_status(ep2);
    if(!ocsq) my_exit("Failed to create status queue!");

    broker_deque_of_outgoing_connection_status_delete(
       broker_outgoing_connection_status_queue_need_pop(ocsq));
    
    broker_string*  msg_str = broker_string_create("Hello, World!\n");
    if(!msg_str) my_exit("Failed to create message string!");

    broker_string*    topic = broker_string_create("test");
    if(!topic) my_exit("Failed to create topic string!");

    broker_data*        msg = broker_data_from_string(msg_str);
    if(!msg) my_exit("Failed to create message data!");

    broker_vector*      vec = broker_vector_create();
    if(!vec) my_exit("Failed to create message vector!");

    res = broker_vector_insert(vec, msg, 0L);
    if(!res) my_exit("Failed to insert into vector!");

    res = broker_endpoint_send(ep1, topic, vec);
    if(!res) my_exit("Failed to send message!");

    //broker_deque_of_message* msg_list = broker_message_queue_want_pop(q);
    broker_deque_of_message* msg_list = broker_message_queue_need_pop(q);
    if(!msg_list) my_exit("Failed to pop queue!");

    size_t num_msgs = broker_deque_of_message_size(msg_list);
    printf("There are %ld messages.\n", num_msgs);
}

