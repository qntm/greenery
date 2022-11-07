# -*- coding: utf-8 -*-

class rxelem:
    '''
        Parent class for all regular expression elements. All elements have
        some things in common. This parent class mainly hosts documentation
        though.
    '''

    def to_fsm(self, alphabet=None):
        '''
            Return the present element in the form of a finite state machine,
            as imported from the `fsm` module.
            If no alphabet is explicitly supplied, which seems quite probable,
            we use the lego.alphabet() method (later) to list all the characters
            mentioned in self. However, if we intend to connect this FSM to another
            one which uses different characters, we may need to supply an alphabet
            which is a superset of both sets.
        '''
        raise NotImplementedError()

    def __repr__(self):
        '''
            Return a string approximating the instantiation line
            for the present lego piece.
        '''
        raise NotImplementedError()

    def __str__(self):
        '''
            Render the present lego piece in the form of a regular expression.
            Some lego pieces may be created which cannot be rendered in this way.
            In particular: a pattern containing no concs; a multiplier of zero.
        '''
        raise NotImplementedError()

    def reduce(self):
        '''
            The most important and algorithmically complex method. Takes the current
            lego piece and simplifies it in every way possible, returning a simpler
            lego piece which is quite probably not of the same class as the original.
            Approaches vary by the class of the present lego piece.

            It is critically important to (1) always call reduce() on whatever you're
            returning before you return it and therefore (2) always return something
            STRICTLY SIMPLER than the current object. Otherwise, infinite loops become
            possible in reduce() calls.
        '''
        raise NotImplementedError()

    def reversed(self):
        '''
            Return a lego object which will match any string which, when reversed,
            self would match. E.g. if self matches "beer" then reversed(self) will
            match "reeb".
        '''
        raise NotImplementedError()

    def __reversed__(self):
        return self.reversed()

    def empty(self):
        '''
            Return False if there exists a string which the present lego piece
            can match. Return True if no such string exists. Examples of empty
            lego pieces are charclass() and pattern()
        '''
        raise NotImplementedError()

    def __hash__(self):
        '''For dictionaries'''
        raise NotImplementedError()

    def alphabet(self):
        '''
            Return a set of all unique characters used in this element.
            In theory this could be a static property, self.alphabet, not
            a function, self.alphabet(), but in the vast majority of cases
            this will never be queried so it's a waste of computation to
            calculate it every time a lego piece is instantiated.
            By convention, fsm.anything_else is always included in this result.
        '''
        raise NotImplementedError()

