/*
This class is an example implementation of the List ADT using a singly linked list.
Taken from my 1A algorithms Java implementation
 */

public class SinglyLinkedList<T> implements List<T>{
    private int mLength;
    private ListNode<T> mHead;


    //class defining each node in the list
    //since singly linked the nodes contain a value and a pointer to the next node
    private class ListNode<T> {
        public T value;
        public ListNode<T> nextElem;
        public ListNode(T x){
            value=x;
        }
    }


    public SinglyLinkedList(){ //create empty list
        mLength = 0;
    }

    public SinglyLinkedList(T x){
        mHead = new ListNode<T>(x);
        mLength = 1;
    }

    @Override
    public boolean isEmpty() {
        return (mLength==0);
    }

    @Override
    public T head() throws UnderflowException {
        if(isEmpty()){
            throw new UnderflowException();
        }
        return mHead.value;
    }

    @Override
    public void prepend(T x) {
        ListNode<T> newHead = new ListNode<T>(x);
        newHead.nextElem = mHead;
        mHead = newHead;
        mLength++;
    }

    @Override
    public List<T> tail() throws UnderflowException {
        if(isEmpty()){
            throw new UnderflowException();
        }
        SinglyLinkedList<T> tail = new SinglyLinkedList<T>();
        ListNode<T> currentElem = mHead;
        //we copy all the elements of the list in order, except the head
        while(currentElem.nextElem!=null){
            tail.append(currentElem.nextElem.value);
            currentElem = currentElem.nextElem;
        }
        return tail;

    }

    @Override
    public void setTail(List<T> newTail) throws UnderflowException {
        if(isEmpty()){
            throw new UnderflowException(); // no head, so cannot set new tail
        }
        mLength=1;
        ListNode<T> currentElem = new ListNode<T>(newTail.head());
        mHead.nextElem = currentElem;
        mLength++;
        while(newTail.tail()!=null){
            newTail = newTail.tail();
            ListNode<T> nextElem = new ListNode<T>(newTail.head());
            currentElem.nextElem = nextElem;
            mLength++;
            currentElem = nextElem;
        }

    }


    public int getLength() {
        return mLength;
    }

    public void append(T x){ //add element to end of list
        ListNode<T> newEnd = new ListNode<T>(x);
        mLength++;
        if(isEmpty()){ //empty list => no head
            mHead= newEnd;
        }
        else {
            ListNode<T> currentElem = mHead;
            while (currentElem.nextElem!=null){
                currentElem = currentElem.nextElem;
            }
            currentElem.nextElem = newEnd;
        }
    }
}