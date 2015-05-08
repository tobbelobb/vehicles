;;;; vehicles.lisp
;; This package describes Vehicle objects and gives means for creating and manipulating them
(defpackage :vehicles/vehicles
  (:use :cl)
  (:export #:test))

;; *** Data flow and hierarchy mapping ***
;; Vehicle objects will be input to World objects
;; World objects will be input to Simulation objects
;; TNN networks will be input to Vehicle brain slot
;; 
;; The function update
;;   should take a World object as its input and return a World where one time step is taken
;;   For all vehicles in World:
;;     Try to execute action in world. Calculate whatever consquences.
;;       (setf World (funcall (planned-action vehicle) World vehicle))
;;   For all vehicles in World:
;;     Update sensory information, store in TNN.
;;       (setf (tnn vehicle) (percieve vehicle World))
;;     Reward
;;       (setf (reward vehicle) (reward-fun vehicle World))
;;     Learn. Reward is used here only. May not need slot in vehicle object.
;;       (setf (tnn vehicle) (update-q (tnn vehicle) (reward vehicle) (planned-action vehicle)))
;;     Associate. Maybe create new concepts. Mark top active ones.
;;       (setf (tnn vehicle) (propagate-perception (tnn vehicle)))
;;     Plan next action
;;       (setf (planned-action vehicle) (plan-action (tnn vehicle)))
;;     Save
;;       (write-data stream World)
;;       
;;     ** Learn **
;;       - Remember previous top active nodes (state) and which action was taken. Possibly several
;;           time steps back.
;;       - Use current reward to update Q-values in TNN. Possibly several time steps back.
;;     ** Associate **
;;       - Maybe create new concepts, store in TNN.
;;       - If new concept was created, make is top-active and children not.
;;     ** Decide **
;;       - Use Q-values to decide which action to take.
;;       - Store planned next action in TNN.
;;     
;;     How do we choose an action when several concept nodes are top active?
;;       - We could sum Q-values from all top active nodes. Greedy action maximizes this sum.
;;     Should we encourage exploration by always increasing unchoosen Q-values a little?
;;       - TODO: Check Peters old mail to see if this was so.
;;  
;; The function update-TNN
;;   should take a TNN object and a reward as its input and update the TNN according to Python
;;   program already written. All temporal information is stored in nodes, so no history-TNN needed.
;;
;; The function Q-update
;;   should take the previous TNN (with the selected action marked) and a reward as its input, and
;;   update the Q-values in the top-active concepts (this is then TD(1))
;;
;; The function run-simulation
;;   should take an :animate key, which shows animation real time if true
;;   it should also take a :save-animation key which saves a video of the simulation
;;
;; *** Two-step simulation model ***
;; File-model 0
;;   run-simulation appends state (World object) to file in data-directory after each update
;;   (write-data).
;;   When simulation is done run-simulation will store current state of Simulation object in
;;   re-loadable fashion in sims-directory
;;   Therefore each simulation must have a name. File name in sims-directory will be
;;   <name>_timestep.sim
;;   TODO: What will be in Simulation object that is not in World object?
;;   
;; There should be a data package
;;   it should contain the functions create-data-file, write-data and read-data
;;   It should be the only link between the Simulate and the Visualize part of the program
;;
;; The function create-data-file
;;   given a file name, it should create the file if it doesn't exist.
;;   All vehicles, sources and other objects in the world should be presented at beginning of file
;;   and given names.
;;
;; The function read-data
;;   given an open file, it should read and return a World object from it.
;;
;; The function write-data
;;   given an open file and a World object, it should append the object to the file the following
;;   way:
;;     For each vehicle in world
;;       write vehicle name
;;       write vehicle position
;;       write vehicle direction
;;       write vehicle speed
;;       write vehicle TNN after updating concepts and deciding action. Concept updates and action
;;         choice must be clearly visible.
;;
;; There should be a visualize package
;;   Visualization is supposed to take place after the simulation is run for a long time.
;;   However, in development process, it is useful to be visualize temp-objects directly for
;;   debugging purposes.
;; The method visualize
;;   given a TNN object, it should return an image of it   (debugging)
;;   given a World object, it should return an image of it (debugging)
;;   given a Simulation object, it should return a video
;;     TODO: This is the only use of Simulation object so far. Do we need it?
;;     We could give visualize a data file name, and have it make a video of that?
;;     That is, Simulation objects could be manifested only as data files.
;;

(in-package :vehicles/vehicles)

(defun test ()
  (print "this is a test"))

