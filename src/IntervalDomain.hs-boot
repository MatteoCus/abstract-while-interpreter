module IntervalDomain where
import Infinitable
data Interval = Empty | Interval (Infinitable Integer) (Infinitable Integer)
