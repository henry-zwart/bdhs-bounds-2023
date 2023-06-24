from .node import Node


class Open_List:
    def __init__(self, f=lambda x: x, tiebreaking="FIFO"):
        self.f = f
        self.queue = (
            []
        )  # the physical representation of the queue index 0 holds the highest priority member
        self.position = (
            {}
        )  # gives index of node in queue this can also be interpreted as the priority

        # LIFO or FIFO
        self.EntryIDs = {}
        self.EntryID = 0
        if tiebreaking == "FIFO":
            self.EntryIDUpdate = 1
        else:
            self.EntryIDUpdate = -1

    def __len__(self):
        """Return current capacity of PriorityQueue."""
        return len(self.queue)

    def __contains__(self, key):  # the in operator
        """Return True if the key is in PriorityQueue."""
        if key in self.position:
            return True
        else:
            return False

    def __getitem__(self, key):
        """Returns the first value associated with key in PriorityQueue.
        Raises KeyError if key is not present."""
        try:
            return self.queue[self.position[key]]
        except ValueError:
            raise KeyError(str(key) + " is not in the priority queue")

    def peek(self):
        return self.queue[0]

    def append(self, obj):
        self.f(obj)
        self.queue.append(obj)
        self.position[obj.state] = self.__len__() - 1  # len is correct here

        self.EntryIDs[obj.state] = self.EntryID
        self.EntryID += self.EntryIDUpdate

        self.percolateUp(self.position[obj.state])
        return

    def pop(self):
        obj = self.queue[0]
        del self.position[obj.state]
        root = self.queue.pop()
        if self.__len__() != 0:  # len is correct here
            self.queue[0] = root
            self.position[root.state] = 0
            self.percolateDown()
        return obj

    # only used for perc down
    def minChild(self, index):
        if index * 2 + 2 >= self.__len__():
            return index * 2 + 1
        else:
            if self.queue[index * 2 + 1].f < self.queue[index * 2 + 2].f:
                return index * 2 + 1
            elif self.queue[index * 2 + 1].f == self.queue[index * 2 + 2].f:
                if self.queue[index * 2 + 1].g > self.queue[index * 2 + 2].g:
                    return index * 2 + 1
            return index * 2 + 2

    def percolateUp(self, index):
        obj = self.queue[index]
        parentIndex = (index - 1) // 2
        parentObj = self.queue[parentIndex]

        while index != 0 and parentObj.f >= obj.f:
            if parentObj.f == obj.f:
                if parentObj.g > obj.g:  # g decreasing
                    break
                if (
                    parentObj.g == obj.g
                    and self.EntryIDs[parentObj.state] < self.EntryIDs[obj.state]
                ):
                    break
            self.position[parentObj.state] = index
            self.queue[index] = parentObj
            index = parentIndex
            parentIndex = (index - 1) // 2
            parentObj = self.queue[parentIndex]

        self.queue[index] = obj
        self.position[obj.state] = index
        return

    def percolateDown(self, index=0):
        while index * 2 + 1 < self.__len__():
            minChildIndex = self.minChild(index)
            child = self.queue[minChildIndex]
            obj = self.queue[index]
            if obj.f >= child.f:
                if obj.f == child.f:
                    if obj.g > child.g:  # g decreasing
                        break
                    if (
                        obj.g == child.g
                        and self.EntryIDs[child.state] < self.EntryIDs[obj.state]
                    ):
                        break
                self.position[obj.state] = minChildIndex
                self.position[child.state] = index
                self.queue[minChildIndex] = obj
                self.queue[index] = child
                index = minChildIndex
            else:
                break
        return

    def decreaseKey(self, newObj):
        # replaces the old node with the new node
        self.f(newObj)
        index = self.position[newObj.state]
        self.queue[index] = newObj

        self.EntryIDs[newObj.state] = self.EntryID
        self.EntryID += self.EntryIDUpdate
        self.percolateUp(index)
        return
