module Icon exposing (..)

import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (d, fill, height, viewBox, width)


icon : String -> Svg msg
icon color =
    svg [ width "120", height "120", viewBox "0 0 16 16", fill color ]
        [ --path [ d "M13.0589 0C12.096 0 11.4301 0.353584 10.9218 0.951486C11.1666 1.08917 11.5148 1.31356 11.8151 1.4777C12.1347 1.17945 12.4803 0.922653 13.0589 0.922653C13.6416 0.922653 14.1494 1.1398 14.447 1.5714C14.7445 2.00299 14.9624 2.64144 14.9624 3.48879C14.9624 4.33218 14.882 4.97064 14.5844 5.40617C14.2869 5.83779 13.8478 6.05491 13.2651 6.05491C10.8853 5.86833 10.9883 2.43754 9.80857 0.872208C9.33771 0.298064 8.6968 0.0144635 7.87761 0.0144167C6.95888 0.0144401 6.22302 0.332386 5.67178 0.965903L6.57198 1.46328C6.90119 1.11484 7.32038 0.944292 7.82951 0.944292C8.35012 0.944292 8.74957 1.1123 9.02519 1.44886C9.91008 2.66088 10.2366 5.09113 11.0249 5.91797C11.5573 6.4765 11.9786 6.87749 12.7497 6.96315H12.7565C12.8908 6.99905 13.3496 7.01344 13.4368 6.98479C14.4018 6.98479 14.8045 6.68493 15.3128 6.09097C15.8211 5.49307 16 4.62519 16 3.48879C16 2.35634 15.681 1.49172 15.1685 0.893819C14.6602 0.295941 14.0218 0 13.0589 0ZM2.61111 0.0712077C1.80901 0.079258 1.18426 0.44119 0.659515 1.03526C0.156904 1.60429 -0.0482976 2.40243 0.00947338 3.51042C0.0213522 3.7406 0.00947338 5.80926 0.00947338 6.84696C0.29282 6.84696 0.663511 6.84696 0.902799 6.84696C0.909193 5.75784 0.889056 5.69333 0.889056 3.51042C0.889053 2.66702 1.01039 1.96989 1.45941 1.54978C1.83971 1.19394 2.1886 0.995244 2.80877 1.02936C3.43167 1.06366 4.00153 1.50856 4.26671 2.09428C4.63957 2.91784 4.37983 4.85401 5.38317 6.01889C5.94589 6.63658 7.02967 6.99921 8.02879 6.99921C8.37714 6.99918 8.73633 6.95655 9.10765 6.86946C9.48279 6.7863 9.57154 6.72976 9.97349 6.55949L9.08016 6.06214C8.94388 6.0656 8.15326 6.14466 8.15935 6.12699C6.90094 6.2191 6.23181 5.63986 5.95352 5.26201C5.8264 5.0894 5.70765 4.89994 5.59619 4.55561C5.55298 4.37133 5.51349 4.18124 5.48625 3.97895C5.41608 3.45786 5.3827 2.89605 5.29384 2.3643C5.20499 1.83257 4.96092 1.39323 4.65683 1.00982C4.19514 0.427764 3.4303 0.0712311 2.61111 0.0712077ZM6.04286 3.05629C6.06859 3.34202 6.04603 3.62838 6.21465 3.91408H8.87401L9.38252 3.48159L8.70909 3.05629H6.04286Z" ] []
          path [ d "M5.52305e-08 14H14.867C14.6937 13.3598 14.3555 12.7697 13.8784 12.2926L12.7929 11.2071C11.3797 9.79392 9.46301 9 7.46447 9H5.52305e-08V14ZM15.8953 14C15.9644 14.3263 16 14.6613 16 15H5.52305e-08C5.52305e-08 12.2663 5.52305e-08 8 5.52305e-08 8H7.46447C9.72823 8 11.8993 8.89928 13.5 10.5L14.5855 11.5855C15.2515 12.2515 15.7031 13.0923 15.8953 14Z" ] []
        , path [ d "M14 14.5C14 15.3284 13.3284 16 12.5 16C11.6716 16 11 15.3284 11 14.5C12 14.5 13 14.5 14 14.5Z" ] []
        , path [ d "M10 14.5C10 15.3284 9.32843 16 8.5 16C7.67157 16 7 15.3284 7 14.5C8 14.5 9 14.5 10 14.5Z" ] []
        , path [ d "M6 12C6 11.4477 6.44772 11 7 11C7.55228 11 8 11.4477 8 12C8 12.5523 7.55228 13 7 13C6.44772 13 6 12.5523 6 12Z" ] []
        , path [ d "M5.52305e-08 11H1C1.55228 11 2 11.4477 2 12C2 12.5523 1.55228 13 1 13H5.52305e-08V11Z" ] []
        , path [ d "M3 11C3 10.4477 3.44772 10 4 10C4.55228 10 5 10.4477 5 11V14H3V11Z" ] []
        , path [ d "M9 11.5C9 10.6716 9.67157 10 10.5 10H11.3836C12.091 10 12.7618 10.3142 13.2147 10.8577L15 13H10.5C9.67157 13 9 12.3284 9 11.5Z" ] []
        ]